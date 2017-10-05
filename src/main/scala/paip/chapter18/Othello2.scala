package paip.chapter18

import paip.chapter18.Othello.Piece._
import paip.chapter18.Othello._

import scala.collection.mutable.ArrayBuffer
import scala.math.BigDecimal.RoundingMode

object Othello2 {
  val allSquares: List[Int] = (11 to 88).filter((i: Int) => {
    val mod = i % 10
    mod >= 1 && mod <= 8
  }).toList.sortWith(weights(_) > weights(_))

  case class Node(square: Option[Int] = None, board: Board, var value: Int)

  /**
    * Return a strategy that does A-B search with sorted moves.
    */
  def alphaBetaSearcher2(depth: Int, evalFn: (Piece, Board) => Int): (Piece, Board) => Int = {
    (player: Piece, board: Board) => {
      val result = alphaBeta2(player,
        Node(board = board, value = evalFn.apply(player, board)),
        LosingValue, WinningValue, depth, evalFn)
      result._2.get.square.get
    }
  }

  /**
    * A-B search, sorting moves by eval-fn
    */
  def alphaBeta2(player: Piece,
                 node: Node,
                 achievable: Int,
                 cutoff: Int,
                 ply: Int,
                 evalFn: (Piece, Board) => Int): (Int, Option[Node]) = {
    if (ply == 0) (node.value, Some(node))
    else {
      val board = node.board
      val nodes = legalNodes(player, board, evalFn)
      if (nodes.isEmpty) {
        if (anyLegalMove(opponent(player), board)) {
          (-alphaBeta2(opponent(player), negateValue(node),
            -cutoff, -achievable, -ply, evalFn)._1,
            None)
        } else {
          (finalValue(player, board), None)
        }
      } else {
        var bestNode = nodes.head
        var achievable_ = achievable
        nodes.iterator.takeWhile((move: Node) => achievable_ >= cutoff).
          foreach((move: Node) => {
            val value = -alphaBeta2(opponent(player), negateValue(move),
              -cutoff, -achievable_, ply - 1, evalFn)._1
            if (value > achievable_) {
              achievable_ = value
              bestNode = move
            }
          })
        (achievable_, Some(bestNode))
      }
    }
  }

  /**
    * A-B search, putting killer move first.
    */
  def alphaBeta3(player: Piece,
                 board: Board,
                 achievable: Int,
                 cutoff: Int,
                 ply: Int,
                 evalFn: (Piece, Board) => Int,
                 killer: Option[Int]): (Int, Option[Int]) = {
    if (ply == 0) (evalFn.apply(player, board), None)
    else {
      val moves = putFirst(killer, legalMoves(player, board))
      if (moves.isEmpty) {
        if (anyLegalMove(player, board))
          (-alphaBeta3(opponent(player), board, -cutoff, -achievable, ply - 1, evalFn, None)._1, None)
        else (finalValue(player, board), None)
      } else {
        var bestMove = moves.head
        val newBoard = PlyBoards(ply)
        var killer2: Option[Int] = None
        var killer2Val = WinningValue
        var achievable_ = achievable
        moves.iterator.takeWhile((i: Int) => achievable_ >= cutoff).
          foreach((move: Int) => {
            val result = alphaBeta3(opponent(player),
              makeMove(move, player, Board(replace(newBoard.pieces.to[ArrayBuffer], board.pieces))),
              -cutoff, -achievable_, ply - 1, evalFn, killer2)
            result match {
              case (value, reply) => {
                if (-value > achievable_) {
                  achievable_ = value
                  bestMove = value
                }
                if (reply.isDefined && value < killer2Val) {
                  killer2 = reply
                  killer2Val = value
                }
              }
            }
          })
        (achievable_, Some(bestMove))
      }
    }
  }

  def replace(ab: ArrayBuffer[Piece], arr: Array[Piece]): Array[Piece] = {
    for (i <- ab.indices) ab(i) = arr(i)
    arr
  }

  /**
    * Return a strategy that does A-B search with killer moves.
    */
  def alphaBetaSearcher3(depth: Int, evalFn: (Piece, Board) => Int): (Piece, Board) => Int = {
    (player: Piece, board: Board) => {
      val result = alphaBeta3(player, board, LosingValue, WinningValue, depth, evalFn, None)
      result._2.get
    }
  }

  /**
    * Move the killer move to the front of moves,
    * if the killer move is in fact a legal move.
    */
  def putFirst(killer: Option[Int], moves: List[Int]): List[Int] = {
    if (killer.isDefined && moves.contains(killer.get))
      killer.get :: moves.filter((m: Int) => !m.equals(killer.get))
    else moves
  }

  def legalNodes(player: Piece, board: Board, evalFn: (Piece, Board) => Int): List[Node] = {
    val moves = legalMoves(player, board)
    moves.map((move: Int) => {
      val newBoard = makeMove(move, player, board.copy())
      Node(Some(move), newBoard, evalFn.apply(player, newBoard))
    }).sortWith(_.value > _.value)
  }

  /**
    * Set the value of a node to its negative.
    */
  def negateValue(node: Node): Node = {
    node.value = -node.value
    node
  }

  val PlyBoards: Array[Board] = Array.fill[Board](40)(initialBoard())

  val neighborTable = NeighborTable(initialBoard())

  /**
    * Current Mobility is the number of legal moves.
    * Potential mobility is the number of blank squares
    * adjacent to an opponent that are not legal moves.
    * Returns current and potential mobility for player.
    */
  def mobility(player: Piece, board: Board): (Int, Int) = {
    val opp = opponent(player)
    var current = 0
    var potential = 0
    for (square <- allSquares) {
      if (aref(square, board).equals(empty)) {
        if (isLegalMove(square, player, board))
          current = current + 1
        else if (neighbors(square, neighborTable).exists((sq: Int) => aref(sq, board).equals(opp)))
          potential = potential + 1
      }
    }
    (current, current + potential)
  }

  /**
    * Array of values to player-to-move for edge positions.
    */
  val EdgeTable: Array[BigDecimal] = Array.fill[BigDecimal](scala.math.pow(3, 10).toInt)(BigDecimal(0.0))

  case class EdgeAndXLists(lsts: List[List[Int]])

  object EdgeAndXLists {
    def apply(lsts: List[Int]*): EdgeAndXLists = EdgeAndXLists(lsts.toList)
  }

  /**
    * The four edges (with their X-squares).
    */
  val edgeAndXLists = EdgeAndXLists(
    List(22, 11, 12, 13, 14, 15, 16, 17, 18, 27),
    List(72, 81, 82, 83, 84, 85, 86, 87, 88, 77),
    List(22, 11, 21, 31, 41, 51, 61, 71, 81, 72),
    List(27, 18, 28, 38, 48, 58, 68, 78, 88, 77))

  val TopEdge: List[Int] = edgeAndXLists.lsts.head

  /**
    * The index counts 1 for player; 2 for opponent,
    * on each square---summed as a base 3 number.
    */
  def edgeIndex(player: Piece, board: Board, squares: List[Int]): Int = {
    var index = 0
    for (sq <- squares) {
      index = index * 3 +
        (if (aref(sq, board).equals(empty)) 0
        else if (aref(sq, board).equals(player)) 1
        else 2)
    }
    index
  }

  /**
    * Total edge evaluation for player to move on board.
    */
  def edgeStability(player: Piece, board: Board): BigDecimal = {
    edgeAndXLists.lsts.map((l: List[Int]) => EdgeTable(edgeIndex(player, board, l))).sum
  }

  /**
    * Initialize *edge-table*, starting from the empty board.
    */
  def initEdgeTable(): Unit = {
    for (npieces <- 0 until 10) {
      mapEdgeNPieces((board: Board, index: Int) => {
        val value = staticEdgeStability(black, board)
        EdgeTable(index) = value
        Some(value)
      }, black, initialBoard(), npieces, TopEdge, 0)
    }

    for (i <- 1 until 5) {
      for (npieces <- 9 to 1 by -1) {
        mapEdgeNPieces((board: Board, index: Int) => {
          val value = possibleEdgeMovesValue(black, board, index)
          EdgeTable(index) = value
          Some(value)
        }, black, initialBoard(), npieces, TopEdge, 0)
      }
    }
  }

  val staticEdgeTable: StaticEdgeTable =
    StaticEdgeTable(
      Array("*", 0 - 2000),
      Array(700, "*", "*"),
      Array(1200, 200, -25),
      Array(1000, 200, 75),
      Array(1000, 200, 50),
      Array(1000, 200, 50),
      Array(1000, 200, 75),
      Array(1200, 200, -25),
      Array(700, "*", "*"),
      Array("*", 0, -2000)
    )

  case class StaticEdgeTable(values: Array[Array[Any]]) {
    def aref(i1: Int, i2: Int): Int = {
      values(i1)(i2).asInstanceOf[Int]
    }
  }

  object StaticEdgeTable {
    def apply(arrs: Array[Any]*): StaticEdgeTable = StaticEdgeTable(arrs.toArray)
  }

  /**
    * Compute this edge's static stability
    */
  def staticEdgeStability(player: Piece, board: Board): Int = {
    TopEdge.zip(0 to TopEdge.size).map((z: (Int, Int)) => {
      if (aref(z._1, board).equals(empty)) 0
      else if (aref(z._1, board).equals(player)) staticEdgeTable.aref(z._2, pieceStability(board, z._1))
      else -staticEdgeTable.aref(z._2, pieceStability(board, z._1))
    }).sum
  }

  val Stable: Int = 0
  val SemiStable: Int = 1
  val Unstable: Int = 2

  def pieceStability(board: Board, sq: Int): Int = {
    if (cornerXsqs.isCorner(sq)) Stable
    else if (cornerXsqs.isXSquare(sq) && cornerXsqs.cornerFor(sq).isDefined)
      if (aref(cornerXsqs.cornerFor(sq).get, board).equals(empty)) Unstable else SemiStable
    else {
      val player = aref(sq, board)
      val opp = opponent(player)
      val p1 = board.pieces.slice(sq, 19).find((p: Piece) => !p.equals(player))
      val p2 = board.pieces.slice(11, sq).reverse.find((p: Piece) => !p.equals(player))

      if (p1.isDefined && p2.isDefined) {
        if ((p1.get.equals(empty) && p2.get.equals(opp)) || (p2.get.equals(empty) && p1.get.equals(opp))) Unstable
        else if (p1.get.equals(opp) && p2.get.equals(opp) &&
          board.pieces.slice(11, 19).exists((p: Piece) => p.equals(empty))) SemiStable
        else if (p1.get.equals(empty) && p2.get.equals(empty)) SemiStable
        else Stable
      } else Stable
    }
  }

  case class CornerXsqs(map: Map[Int, Int]) {
    def isCorner(sq: Int): Boolean = map.keySet.contains(sq)

    def isXSquare(sq: Int): Boolean = map.values.exists(_ == sq)

    def xSquareFor(corner: Int): Option[Int] = map.get(corner)

    def cornerFor(xsq: Int): Option[Int] = {
      val pair = map.find((e: (Int, Int)) => e._2 == xsq)
      if (pair.isDefined) Some(pair.get._2)
      else None
    }
  }

  val cornerXsqs: CornerXsqs = CornerXsqs((11, 22), (18, 27), (81, 72), (88, 77))

  object CornerXsqs {
    def apply(values: (Int, Int)*): CornerXsqs = new CornerXsqs(values.toMap)
  }

  /**
    * Call fn on all edges with n pieces.
    */
  def mapEdgeNPieces(fn: (Board, Int) => Option[Int], player: Piece, board: Board, n: Int, squares: List[Int], index: Int): Option[Int] = {
    if (squares.length < n) None
    else if (squares.isEmpty) fn.apply(board, index)
    else {
      val index3 = 3 * index
      val sq = squares.head
      mapEdgeNPieces(fn, player, board, n, squares.tail, index3)
      if (n > 0 && aref(sq, board).equals(empty)) {
        aset(sq, player, board)
        mapEdgeNPieces(fn, player, board, n - 1, squares.tail, 1 + index3)
        aset(sq, opponent(player), board)
        mapEdgeNPieces(fn, player, board, n - 1, squares.tail, 2 + index3)
        aset(sq, empty, board)
        Some(empty.id)
      } else None
    }
  }

  /**
    * Combine the best moves.
    */
  def combineEdgeMoves(possibilities: List[List[BigDecimal]], player: Piece): Int = {
    var prob = BigDecimal(1.0)
    var value = BigDecimal(0.0)
    val fn: (BigDecimal, BigDecimal) => Boolean =
      if (player.equals(black))
        (b1: BigDecimal, b2: BigDecimal) => b1 > b2
      else
        (b1: BigDecimal, b2: BigDecimal) => b1 < b2

    val sorted = possibilities.sortWith((l1: List[BigDecimal], l2: List[BigDecimal]) => {
      fn.apply(l1.tail.head, l2.tail.head)
    })

    sorted.iterator.takeWhile((l: List[BigDecimal]) => prob >= BigDecimal(0.0)).
      foreach((pair: List[BigDecimal]) => {
        value = value + (prob * pair.head * pair.tail.head)
        prob = prob - (prob * pair.head)
      })

    value.setScale(0, RoundingMode.HALF_UP).toInt
  }

  /**
    * Count the neighbors of this square occupied by player.
    */
  def countEdgeNeighbors(player: Piece, board: Board, square: Int): Int = {
    countIf((inc: Int) => aref(square + inc, board).equals(player), List(1, -1))
  }

  /**
    * What's the probability that player can move to this square?
    */
  def edgeMoveProbability(player: Piece, board: Board, square: Int): BigDecimal = {
    if (cornerXsqs.isXSquare(square)) BigDecimal(0.5)
    else if (isLegalMove(square, player, board)) BigDecimal(1.0)
    else if (cornerXsqs.isCorner(square)) {
      val xSq = cornerXsqs.xSquareFor(square)
      if (xSq.isDefined && aref(xSq.get, board).equals(empty)) BigDecimal(0.1)
      else if (xSq.isDefined && aref(xSq.get, board).equals(player)) BigDecimal(0.001)
      else BigDecimal(0.9)
    }
    else {
      val i1 = countEdgeNeighbors(player, board, square)
      val i2 = countEdgeNeighbors(opponent(player), board, square)
      val arr = Array(
        Array(BigDecimal(0.1), BigDecimal(0.4), BigDecimal(0.7)),
        Array(BigDecimal(0.05), BigDecimal(0.3), None),
        Array(BigDecimal(0.01), None, None)
      )
      arr(i1)(i2).asInstanceOf[BigDecimal] / (if (isLegalMove(square, opponent(player), board)) 1 else 2)
    }
  }

  /**
    * Consider all possible edge moves.
    * Combine their values into a single number.
    */
  def possibleEdgeMovesValue(player: Piece, board: Board, index: Int): Int = {
    val possibilities = List(BigDecimal(1.0), EdgeTable(index)) ::
      TopEdge.filter((sq: Int) => aref(sq, board).equals(empty)).map((sq: Int) => possibleEdgeMove(player, board, sq))
    combineEdgeMoves(possibilities, player)
  }

  /**
    * Return a (prob val) pair for a possible edge move.
    */
  def possibleEdgeMove(player: Piece, board: Board, sq: Int): List[BigDecimal] = {
    val newBoard = Board(replace(PlyBoards(player.id).pieces.to[ArrayBuffer], board.pieces))
    makeMove(sq, player, newBoard)
    List(edgeMoveProbability(player, board, sq), -EdgeTable(edgeIndex(opponent(player), newBoard, TopEdge)))
  }

  val MoveNumber: Int = 1

  /**
    * Combine edge-stability, current mobility and
    * potential mobility to arrive at an evaluation.
    */
  def iagoEval(player: Piece, board: Board): Int = {
    val cEdg = 312000 + 6240 * MoveNumber
    val cCur = if (MoveNumber < 25) 6240 + (2000 * MoveNumber) else 75000 + (1000 * MoveNumber)
    val cPot = 20000

    val (pCur, pPot) = mobility(player, board)
    val (oCur, oPot) = mobility(opponent(player), board)

    val ret1 = cEdg * edgeStability(player, board) * 32000
    val ret2 = cCur * (pCur - oCur) * (pCur + oCur + 2)
    val ret3 = cPot * (pPot - oPot) * (pPot + oPot + 2)

    ret1.setScale(0, RoundingMode.HALF_UP).toInt + ret2 + ret3
  }

  /**
    * Use an approximation of Iago's evaluation function.
    */
  def iago(depth: Int): (Piece, Board) => Int = {
    alphaBetaSearcher3(depth, iagoEval)
  }

  def main(args: Array[String]): Unit = {
    initEdgeTable()
  }

}
