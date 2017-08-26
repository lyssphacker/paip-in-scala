package paip.chapter18

import scala.util.Random

object Othello {

  object Piece extends Enumeration {
    type Piece = Value
    val empty = Value(".")
    val black = Value("@")
    val white = Value("0")
    val outer = Value("?")
  }

  val allDirections = List(-11, -10, -9, -1, 1, 9, 10, 11)

  val allSquares = (11 to 88).filter((i: Int) => {
    val mod = i % 10
    mod >= 1 && mod <= 8
  }).toList

  import paip.chapter18.Othello.Piece._

  def opponent(player: Piece): Piece = {
    if (player.equals(black)) white
    else black
  }

  case class Board(pieces: Array[Piece]) {
    def aref(square: Int): Piece = {
      pieces(square)
    }

    def aset(square: Int, value: Piece): Unit = {
      pieces(square) = value
    }

    def count(p: Piece): Int = {
      pieces.toList.count(_.equals(p))
    }

    def printBoard(): Unit = {
      println(s"${" " * 3} 1 2 3 4 5 6 7 8 [$black=${count(black)} " +
        s"$white=${count(white)} (${countDifference(black, this)})]")

      for (row <- 1 to 8) {
        print(s"${10 * row}  ")
        for (col <- 1 to 8) {
          print(s"${aref(col + row * 10)} ")
        }
        println()
      }
      println()
    }

    def findBracketingPiece(square: Int, player: Piece, dir: Int): Option[Int] = {
      if (aref(square).equals(player)) Some(square)
      else if (aref(square).equals(opponent(player))) findBracketingPiece(square + dir, player, dir)
      else None
    }

    def wouldFlip(move: Int, player: Piece, dir: Int): Option[Int] = {
      val c = move + dir
      if (aref(c).equals(opponent(player))) findBracketingPiece(c + dir, player, dir)
      else None
    }

    def makeFlips(move: Int, player: Piece, dir: Int): Unit = {
      val bracketer = wouldFlip(move, player, dir)
      if (bracketer.isDefined) {
        for (c <- (move + dir) to bracketer.get by dir) {
          aset(c, player)
        }
      }
    }

    def anyLegalMove(player: Piece): Boolean = {
      allSquares.exists(isLegalMove(_, player))
    }

    def isLegalMove(move: Int, player: Piece): Boolean = {
      aref(move).equals(empty) &&
        allDirections.exists(wouldFlip(move, player, _).isDefined)
    }

    def nextToPlay(previousPlayer: Piece, print: Boolean): Option[Piece] = {
      val opp = opponent(previousPlayer)
      if (anyLegalMove(opp)) Some(opp)
      else if (anyLegalMove(previousPlayer)) {
        if (print) println(s"$opp has no moves and must pass.")
        Some(previousPlayer)
      }
      else None
    }

    def getMove(strategy: (Piece, Board) => Int, player: Piece, print: Boolean): Board = {
      if (print) printBoard()
      val move = strategy.apply(player, this.copy())
      if (isValidMove(move) && isLegalMove(move, player)) {
        if (print) println(s"$player moves to $move")
        makeMove(move, player)
      } else {
        println(s"Illegal move: $move")
        getMove(strategy, player, print)
      }
    }

    def makeMove(move: Int, player: Piece): Board = {
      aset(move, player)
      for (dir <- allDirections) makeFlips(move, player, dir)
      this
    }

    def finalValue(player: Piece): Int = {
      countDifference(player, this) match {
        case -1 => Board.LosingValue
        case 0 => 0
        case 1 => Board.WinningValue
      }
    }
  }

  object Board {
    val WinningValue = Int.MaxValue
    val LosingValue = Int.MinValue
  }

  def isValidMove(move: Int): Boolean = {
    val mod = move % 10
    move >= 11 && move <= 88 && mod >= 1 && mod <= 8
  }

  def countDifference(p: Piece, board: Board): Int = {
    board.count(p) - board.count(opponent(p))
  }

  def initialBoard(): Board = {
    val board = Board(Array.fill[Piece](100)(outer))
    for (square <- allSquares) board.aset(square, empty)
    board.aset(44, white)
    board.aset(45, black)
    board.aset(54, black)
    board.aset(55, white)
    board
  }

  def othello(blStrategy: (Piece, Board) => Int, whStrategy: (Piece, Board) => Int, print: Boolean = true): Int = {
    val board = initialBoard()
    var player: Option[Piece] = Some(black)
    do {
      val strategy = if (player.get.equals(black)) blStrategy else whStrategy
      board.getMove(strategy, player.get, print)
      player = board.nextToPlay(player.get, print)
    } while (player.isDefined)
    if (print) {
      println("The game is over. Final result: ")
      board.printBoard()
    }
    countDifference(black, board)
  }

  def human(player: Piece, board: Board): Int = {
    println(s"$player to move: ")
    scala.io.StdIn.readInt()
  }

  def legalMoves(player: Piece, board: Board): List[Int] = {
    allSquares.filter((m: Int) => board.isLegalMove(m, player))
  }

  def randomStrategy(player: Piece, board: Board): Int = {
    val moves = legalMoves(player, board)
    moves(Random.nextInt(moves.size))
  }

  def maximizier(evalFn: (Piece, Board) => Int): (Piece, Board) => Int = {
    (player: Piece, board: Board) => {
      val moves = legalMoves(player, board)
      val scores = moves.map((m: Int) => evalFn.apply(player, board.copy().makeMove(m, player)))
      val best = scores.max
      moves(scores.indexOf(best))
    }
  }

  def maximizeDifference(player: Piece, board: Board): Int = {
    maximizier(countDifference).apply(player, board)
  }

  val weights = Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 120, -20, 20, 5, 5, 20, -20, 120, 0,
    0, -20, -40, -5, -5, -5, -5, -40, -20, 0,
    0, 20, -5, 15, 3, 3, 15, -5, 20, 0,
    0, 5, -5, 3, 3, 3, 3, -5, 5, 0,
    0, 5, -5, 3, 3, 3, 3, -5, 5, 0,
    0, 20, -5, 15, 3, 3, 15, -5, 20, 0,
    0, -20, -40, -5, -5, -5, -5, -40, -20, 0,
    0, 120, -20, 20, 5, 5, 20, -20, 120, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

  def weightedSquares(player: Piece, board: Board): Int = {
    val opp = opponent(player)
    val sum1 = allSquares.filter((s: Int) => player.equals(board.aref(s))).map(weights(_)).sum
    val sum2 = allSquares.filter((s: Int) => opp.equals(board.aref(s))).map(-weights(_)).sum
    sum1 + sum2
  }

  def minimax(player: Piece, board: Board, ply: Int, evalFn: (Piece, Board) => (Option[Int], Option[Int])): (Option[Int], Option[Int]) = {
    if (ply == 0) evalFn.apply(player, board)
    else {
      val moves = legalMoves(player, board)
      if (moves.isEmpty) {
        if (board.anyLegalMove(opponent(player))) {
          val result = minimax(opponent(player), board, ply - 1, evalFn)
          val first = result._1.get
          (Some(-first), None)
        } else (Some(board.finalValue(player)), None)
      } else {
        var bestMove: Option[Int] = None
        var bestVal: Option[Int] = None
        for (move <- moves) {
          val board2 = board.copy().makeMove(move, player)
          val value = -minimax(opponent(player), board2, ply - 1, evalFn)._1.get
          if (bestVal.isEmpty || value > bestVal.get) {
            bestVal = Some(value)
            bestMove = Some(move)
          }
        }
        (bestVal, bestMove)
      }
    }
  }

  def minimaxSearcher(ply: Int, evalFn: (Piece, Board) => (Option[Int], Option[Int])): (Piece, Board) => Int = {
    (player: Piece, board: Board) => {
      val result = minimax(player, board, ply, evalFn)
      result._1.get
    }
  }

  def alphaBeta(player: Piece, board: Board, achievable: Int, cutoff: Int,
                ply: Int, evalFn: (Piece, Board) => (Option[Int], Option[Int])): (Option[Int], Option[Int]) = {
    if (ply == 0) evalFn.apply(player, board)
    else {
      val moves = legalMoves(player, board)
      if (moves.isEmpty) {
        if (board.anyLegalMove(opponent(player))) {
          val result = alphaBeta(opponent(player), board, -cutoff, -achievable, ply - 1, evalFn)
          val first = result._1.get
          (Some(-first), None)
        } else (Some(board.finalValue(player)), None)
      } else {
        var bestMove = moves.head
        var achievable_ = achievable
        moves.iterator.takeWhile((i: Int) => achievable_ >= cutoff).
          foreach((move: Int) => {
            val board2 = board.copy().makeMove(move, player)
            val result = alphaBeta(opponent(player), board2, -cutoff, -achievable_, ply - 1, evalFn)
            val value = -result._1.get
            if (value > achievable_) {
              achievable_ = value
              bestMove = move
            }
          })
        (Some(achievable_), Some(bestMove))
      }
    }
  }

  def alphaBetaSearcher(depth: Int, evalFn: (Piece, Board) => (Option[Int], Option[Int])): (Piece, Board) => Int = {
    (player: Piece, board: Board) => {
      val result = alphaBeta(player, board, Board.LosingValue, Board.WinningValue, depth, evalFn)
      result._2.get
    }
  }

  def adaptFn(fn: (Piece, Board) => Int): (Piece, Board) => (Option[Int], Option[Int]) = {
    (player: Piece, board: Board) => {
      (Some(fn.apply(player, board)), None)
    }
  }

  def modifiedWeightedSquares(player: Piece, board: Board): Int = {
    var w = weightedSquares(player, board)
    val neighborTable = NeighborTable()
    List(11, 18, 81, 88).foreach((corner: Int) => {
      if (!board.aref(corner).equals(empty))
        for (c <- neighborTable.neighbors(corner))
          if (!board.aref(c).equals(empty))
            w += (5 - weights(c)) * (if (board.aref(c).equals(player)) 1 else -1)
    })
    w
  }

  case class NeighborTable(squares: Array[List[Int]]) {
    def neighbors(square: Int): List[Int] = {
      squares(square)
    }
  }

  object NeighborTable {
    def apply(): NeighborTable = {
      val squares = new Array[List[Int]](100)
      for (square <- allSquares)
        for (dir <- allDirections)
          if (isValidMove(square + dir))
            squares(square) = squares(square) :+ (square + dir)

      NeighborTable(squares)
    }
  }

  def main(args: Array[String]): Unit = {
//        othello(human, human)
    othello(alphaBetaSearcher(6, adaptFn(countDifference)), alphaBetaSearcher(4, adaptFn(weightedSquares)))
  }
}
