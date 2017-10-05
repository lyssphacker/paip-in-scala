package paip.chapter18

import scala.util.Random

object Othello {

  object Piece extends Enumeration {
    type Piece = Value
    val empty: Piece = Value(".") // An empty square
    val black: Piece = Value("@") // A black piece
    val white: Piece = Value("0") // A white piece
    val outer: Piece = Value("?") // Marks squares outside the 8x8 board
  }

  val allDirections = List(-11, -10, -9, -1, 1, 9, 10, 11)

  val allSquares: List[Int] = (11 to 88).filter((i: Int) => {
    val mod = i % 10
    mod >= 1 && mod <= 8
  }).toList

  import paip.chapter18.Othello.Piece._

  val opponent: PartialFunction[Piece, Piece] = {
    case p: Piece if p.equals(black) => white
    case p: Piece if p.equals(white) => black
  }

  /**
    * Game board.
    */
  case class Board(pieces: Array[Piece])

  object Board {
    val WinningValue: Int = Int.MaxValue
    val LosingValue: Int = Int.MinValue
  }

  /**
    * Return the square number of the bracketing piece.
    */
  def findBracketingPiece(board: Board, square: Int, player: Piece, dir: Int): Option[Int] = {
    if (aref(square, board).equals(player)) Some(square)
    else if (aref(square, board).equals(opponent(player))) findBracketingPiece(board, square + dir, player, dir)
    else None
  }

  def aref(square: Int, board: Board): Piece = {
    board.pieces(square)
  }

  /**
    * Would this move result in any flips in this direction?
    * If so, return the square number of the bracketing piece.
    */
  def wouldFlip(board: Board, move: Int, player: Piece, dir: Int): Option[Int] = {
    val c = move + dir
    if (aref(c, board).equals(opponent(player))) findBracketingPiece(board, c + dir, player, dir)
    else None
  }

  /**
    * Print a board, along with some statistics.
    */
  def printBoard(board: Board, clock: Clock = Clock.empty): Unit = {
    print(s"${" " * 4} a b c d e f g h [$black=${count(black, board)} " +
      s"$white=${count(white, board)} (${countDifference(black, board)})]")

    for (row <- 1 to 8) {
      println()
      print(s"  $row  ")
      for (col <- 1 to 8) {
        print(s"${aref(col + row * 10, board)} ")
      }
    }
    if (!clock.isEmpty) {
      print(s"  [$black = ${clock.toMinsSecs(black.id)} $white = ${clock.toMinsSecs(white.id)}]")
    }
    println()
    println()
  }

  def aset(square: Int, value: Piece, board: Board): Unit = {
    board.pieces(square) = value
  }

  def count(p: Piece, board: Board): Int = {
    board.pieces.toList.count(_.equals(p))
  }

  /**
    * Make any flips in the given direction.
    */
  def makeFlips(move: Int, player: Piece, board: Board, dir: Int): Unit = {
    val bracketer = wouldFlip(board, move, player, dir)
    if (bracketer.isDefined) {
      for (c <- (move + dir) until bracketer.get by dir) {
        aset(c, player, board)
      }
    }
  }

  /**
    * Does player have any legal moves in this position?
    */
  def anyLegalMove(player: Piece, board: Board): Boolean = {
    allSquares.exists(isLegalMove(_, player, board))
  }

  /**
    * A Legal move must be into an empty square, and it must
    * flip at least one opponent piece.
    */
  def isLegalMove(move: Int, player: Piece, board: Board): Boolean = {
    aref(move, board).equals(empty) &&
      allDirections.exists(wouldFlip(board, move, player, _).isDefined)
  }

  /**
    * Compute the player to move next, or NIL if nobody can move.
    */
  def nextToPlay(previousPlayer: Piece, board: Board, print: Boolean): Option[Piece] = {
    val opp = opponent(previousPlayer)
    if (anyLegalMove(opp, board)) Some(opp)
    else if (anyLegalMove(previousPlayer, board)) {
      if (print) println(s"$opp has no moves and must pass.")
      Some(previousPlayer)
    }
    else None
  }

  /**
    * Update board to reflect move by player
    */
  def makeMove(move: Int, player: Piece, board: Board): Board = {
    aset(move, player, board)
    for (dir <- allDirections) makeFlips(move, player, board, dir)
    board
  }

  /**
    * Is this a win, loss, or draw for player?
    */
  def finalValue(player: Piece, board: Board): Int = {
    val diff = countDifference(player, board)
    if (diff > 0) Board.WinningValue
    else if (diff < 0) Board.LosingValue
    else 0
  }

  def copyBoard(board: Board) = board.copy(board.pieces.clone)

  /**
    * Valid moves are numbers in the range 11-88 that end in 1-8.
    */
  def isValidMove(move: Int, board: Board): Boolean = {
    val mod = move % 10
    move >= 11 && move <= 88 && mod >= 1 && mod <= 8
  }

  /**
    * Count player's pieces minus opponent's pieces.
    */
  def countDifference(p: Piece, board: Board): Int = {
    count(p, board) - count(opponent(p), board)
  }

  /**
    * Return a board, empty except for four pieces in the middle.
    */
  def initialBoard(): Board = {
    val board = Board(Array.fill[Piece](100)(outer))
    for (square <- allSquares) aset(square, empty, board)
    aset(44, white, board)
    aset(45, black, board)
    aset(54, black, board)
    aset(55, white, board)
    board
  }

  case class Clock(values: Array[Int]) {
    def decf(player: Int, amount: Long): Unit = {
      values(player) = values(player) - amount.toInt
    }

    def elt(player: Int): Int = values(player)

    def isEmpty: Boolean = values.isEmpty

    def toMinsSecs(player: Int): String = {
      val mins = elt(player) / 60000
      val secs = (elt(player) % 60000) / 1000

      f"$mins%02d:$secs%02d"
    }
  }

  /**
    * Game clock.
    */
  object Clock {
    def apply(minutes: Int): Clock = Clock(Array.fill[Int](List(black.id, white.id).max + 1)(minutes * 60000))

    def empty: Clock = Clock(Array[Int]())
  }

  case class OthelloException(result: Int) extends Exception

  var MoveNumber = 1

  /**
    * Play a game of othello.  Return the score, where a positive
    * difference means black, the first player, wins.
    */
  def othello(blStrategy: (Piece, Board) => Option[Either[Int, String]],
              whStrategy: (Piece, Board) => Option[Either[Int, String]],
              print: Boolean = true,
              minutes: Int = 30): Int = {
    val board = initialBoard()
    val clock = Clock(minutes)
    MoveNumber = 1
    try {
      var player: Option[Piece] = Some(black)
      do {
        val strategy = if (player.get.equals(black)) blStrategy else whStrategy
        getMove(strategy, player.get, board, print, clock)
        player = nextToPlay(player.get, board, print)
        MoveNumber = MoveNumber + 1
      } while (player.isDefined)
    } catch {
      case ex: OthelloException => return ex.result
    }
    if (print) {
      println("The game is over. Final result:")
      printBoard(board, clock)
    }
    countDifference(black, board)
  }

  val squareNames: SquareNames = SquareNames()

  /**
    * Call the player's strategy function to get a move.
    * Keep calling until a legal move is made.
    */
  def getMove(strategy: (Piece, Board) => Option[Either[Int, String]], player: Piece, board: Board, print: Boolean, clock: Clock): Board = {
    if (print) printBoard(board, clock)
    val t0 = System.currentTimeMillis()
    val move = strategy.apply(player, board)
    val t1 = System.currentTimeMillis()
    clock.decf(player.id, t1 - t0)
    if (clock.elt(player.id) < 0) {
      println(s"$player has no time left and forfeits.")
      throw OthelloException(if (player.equals(black)) -64 else 64)
    } else if (move.isDefined && move.get.isRight && move.get.right.get.equals("resign")) {
      throw OthelloException(if (player.equals(black)) -64 else 64)
    } else if (move.isDefined && move.get.isLeft && isValidMove(move.get.left.get, board) && isLegalMove(move.get.left.get, player, board)) {
      if (print) println(s"$player moves to ${squareNames.numericToAlpha(move.get.left.get, board).right.get}")
      makeMove(move.get.left.get, player, board)
    } else {
      move match {
        case Some(Left(i)) =>
          squareNames.numericToAlpha(i, board) match {
            case Left(j) => println(s"Illegal move: $j")
            case Right(s) => println(s"Illegal move: $s")
          }
        case Some(Right(s)) => println(s"Illegal move: $s")
        case None =>
      }
      getMove(strategy, player, board, print, clock)
    }
  }

  /**
    * A human player for the game of Othello
    */
  def human(player: Piece, board: Board): Int = {
    println(s"$player to move: ${legalMoves(player, board).map((m: Int) => squareNames.numericToAlpha(m, board).right.get).mkString(" ")}")
    var result: Option[Int] = None
    do {
      squareNames.alphaToNumeric(scala.io.StdIn.readLine()).left.getOrElse(None) match {
        case i: Int => result = Some(i)
        case None => println("Non existent command.")
      }
    } while (result.isEmpty)

    result.get
  }

  /**
    * Returns a list of legal moves for player
    */
  def legalMoves(player: Piece, board: Board): List[Int] = {
    allSquares.filter((m: Int) => isLegalMove(m, player, board))
  }

  /**
    * "Make any legal move.
    */
  def randomStrategy(player: Piece, board: Board): Option[Either[Int, String]] = {
    val moves = legalMoves(player, board)
    Some(Left(moves(GlobalRandom.nextInt(moves.size))))
  }

  /**
    * Return a strategy that will consider every legal move,
    * apply EVAL-FN to each resulting board, and choose
    * the move for which EVAL-FN returns the best score.
    * FN takes two arguments: the player-to-move and board
    */
  def maximizier(evalFn: (Piece, Board) => Int): (Piece, Board) => Int = {
    (player: Piece, board: Board) => {
      val moves = legalMoves(player, board)
      val scores = moves.map((m: Int) => evalFn.apply(player, makeMove(m, player, copyBoard(board))))
      val best = scores.max
      moves(scores.indexOf(best))
    }
  }

  /**
    * A strategy that maximizes the difference in pieces.
    */
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

  /**
    * Sum of the weights of player's squares minus opponent's.
    */
  def weightedSquares(player: Piece, board: Board): Int = {
    val opp = opponent(player)
    val sum1 = allSquares.filter((s: Int) => player.equals(aref(s, board))).map(weights(_)).sum
    val sum2 = allSquares.filter((s: Int) => opp.equals(aref(s, board))).map(-weights(_)).sum
    sum1 + sum2
  }

  /**
    * Find the best move, for PLAYER, according to EVAL-FN,
    * searching PLY levels deep and backing up values.
    */
  def minimax(player: Piece,
              board: Board,
              ply: Int,
              evalFn: (Piece, Board) => (Int, Option[Either[Int, String]])): (Int, Option[Either[Int, String]]) = {
    if (ply == 0) evalFn.apply(player, board)
    else {
      val moves = legalMoves(player, board)
      if (moves.isEmpty) {
        if (anyLegalMove(opponent(player), board)) {
          val result = minimax(opponent(player), board, ply - 1, evalFn)
          (-result._1, None)
        } else (finalValue(player, board), None)
      } else {
        var bestMove: Option[Either[Int, String]] = None
        var bestVal: Option[Int] = None
        for (move <- moves) {
          val board2 = makeMove(move, player, copyBoard(board))
          val value = -minimax(opponent(player), board2, ply - 1, evalFn)._1
          if (bestVal.isEmpty || value > bestVal.get) {
            bestVal = Some(value)
            bestMove = Some(Left(move))
          }
        }
        (bestVal.get, bestMove)
      }
    }
  }

  /**
    * A strategy that searches PLY levels and then uses EVAL-FN.
    */
  def minimaxSearcher(ply: Int,
                      evalFn: (Piece, Board) => (Int, Option[Either[Int, String]])): (Piece, Board) => Option[Either[Int, String]] = {
    (player: Piece, board: Board) => {
      minimax(player, board, ply, evalFn)._2
    }
  }

  /**
    * Find the best move, for PLAYER, according to EVAL-FN,
    * searching PLY levels deep and backing up values,
    * using cutoffs whenever possible.
    */
  def alphaBeta(player: Piece, board: Board, achievable: Int, cutoff: Int,
                ply: Int, evalFn: (Piece, Board) => (Int, Option[Either[Int, String]])): (Int, Option[Either[Int, String]]) = {
    if (ply == 0) evalFn.apply(player, board)
    else {
      val moves = legalMoves(player, board)
      if (moves.isEmpty) {
        if (anyLegalMove(opponent(player), board)) {
          val result = alphaBeta(opponent(player), board, -cutoff, -achievable, ply - 1, evalFn)
          (-result._1, None)
        } else (finalValue(player, board), None)
      } else {
        var bestMove = moves.head
        var achievable_ = achievable
        var i = 0
        do {
          val board2 = makeMove(moves(i), player, copyBoard(board))
          val result = alphaBeta(opponent(player), board2, -cutoff, -achievable_, ply - 1, evalFn)
          val value = -result._1
          if (value > achievable_) {
            achievable_ = value
            bestMove = moves(i)
          }
          i = i + 1
        } while (i < moves.size && achievable_ < cutoff)
        (achievable_, Some(Left(bestMove)))
      }
    }
  }

  /**
    * A strategy that searches to DEPTH and then uses EVAL-FN.
    */
  def alphaBetaSearcher(depth: Int,
                        evalFn: (Piece, Board) => (Int, Option[Either[Int, String]])): (Piece, Board) => Option[Either[Int, String]] = {
    (player: Piece, board: Board) => {
      val result = alphaBeta(player, board, Board.LosingValue, Board.WinningValue, depth, evalFn)
      result._2
    }
  }

  def adaptEvalFn(fn: (Piece, Board) => Int): (Piece, Board) => (Int, Option[Either[Int, String]]) = {
    (player: Piece, board: Board) => {
      (fn.apply(player, board), None)
    }
  }

  /**
    * Like WEIGHTED-SQUARES, but don't take off for moving
    * near an occupied corner.
    */
  def modifiedWeightedSquares(player: Piece, board: Board): Int = {
    var w = weightedSquares(player, board)
    val neighborTable = NeighborTable(board)
    List(11, 18, 81, 88).foreach((corner: Int) => {
      if (!aref(corner, board).equals(empty))
        for (c <- neighbors(corner, neighborTable))
          if (!aref(c, board).equals(empty))
            w += (5 - weights(c)) * (if (aref(c, board).equals(player)) 1 else -1)
    })
    w
  }

  case class NeighborTable(squares: Array[List[Int]])

  object NeighborTable {
    def apply(board: Board): NeighborTable = {
      val squares = Array.fill[List[Int]](100)(List())
      for (square <- allSquares)
        for (dir <- allDirections)
          if (isValidMove(square + dir, board))
            squares(square) = squares(square) :+ (square + dir)

      NeighborTable(squares)
    }
  }

  /**
    * Return a list of all squares adjacent to a square.
    */
  def neighbors(square: Int, neighborTable: NeighborTable): List[Int] = {
    neighborTable.squares(square)
  }

  case class SquareNames(names: List[String]) {
    /**
      * Convert from alphanumeric to numeric square notation.
      */
    def alphaToNumeric(str: String): Either[Int, String] = {
      val position = names.indexOf(str)
      if (position == -1) Right(str)
      else Left(position)
    }

    /**
      * Convert from numeric to alphanumeric square notation.
      */
    def numericToAlpha(num: Int, board: Board): Either[Int, String] = {
      if (isValidMove(num, board)) Right(names(num))
      else Left(num)
    }
  }

  object SquareNames {
    def apply(): SquareNames = {
      SquareNames(crossProduct((x: String, y: String) => x + y,
        List("?", "a", "b", "c", "d", "e", "f", "g", "h", "?"),
        List("?", "1", "2", "3", "4", "5", "6", "7", "8", "?")))
    }

    /**
      * Return a list of all (fn x y) values.
      */
    def crossProduct(fn: (String, String) => String, xlist: List[String], ylist: List[String]): List[String] = {
      ylist.flatMap((y: String) => xlist.map((x: String) => fn.apply(x, y)))
    }
  }

  def countIf(fn: Int => Boolean, lst: List[Int]): Int = {
    lst.count(fn)
  }

  def isPositive(i: Int): Boolean = i > 0

  def isZero(i: Int): Boolean = i == 0

  /**
    * Play a series of 2*n-pairs games, swapping sides.
    */
  def othelloSeries(strategy1: (Piece, Board) => Option[Either[Int, String]],
                    strategy2: (Piece, Board) => Option[Either[Int, String]],
                    npairs: Int): (Float, Int, List[Int]) = {
    var scores: List[Int] = List.empty
    1 to npairs foreach ((i: Int) => {
      scores = othello(strategy1, strategy2) +: scores
      GlobalRandom = new Random()
      scores = -othello(strategy2, strategy1) +: scores
    })

    val res1 = countIf(isPositive, scores) + countIf(isZero, scores).toFloat / 2
    val res2 = scores.sum

    (res1, res2, scores)
  }

  var GlobalRandom: Random = new Random()

  /**
    * Play a series of 2*n games, starting from a random position.
    */
  def randomOthelloSeries(strategy1: (Piece, Board) => Option[Either[Int, String]],
                          strategy2: (Piece, Board) => Option[Either[Int, String]],
                          npairs: Int,
                          nrandom: Int = 10): (Float, Int, List[Int]) = {
    othelloSeries(switchStrategies(randomStrategy, nrandom, strategy1),
      switchStrategies(randomStrategy, nrandom, strategy2),
      npairs)
  }

  /**
    * Make a new strategy that plays strategy1 for m moves,
    * then plays according to strategy2.
    */
  def switchStrategies(strategy1: (Piece, Board) => Option[Either[Int, String]],
                       m: Int,
                       strategy2: (Piece, Board) => Option[Either[Int, String]]): (Piece, Board) => Option[Either[Int, String]] = {
    (player: Piece, board: Board) => (if (MoveNumber <= m) strategy1 else strategy2).apply(player, board)
  }

  /**
    * Play a tournament among the strategies.
    * N-PAIRS = games each strategy plays as each color against
    * each opponent.  So with N strategies, a total of
    * N*(N-1)*N-PAIRS games are played.
    */
  def roundRobin(strategies: List[(Piece, Board) => Option[Either[Int, String]]],
                 npairs: Int,
                 nrandom: Int = 10,
                 names: List[String]): Unit = {
    val n = strategies.length
    val totals = Array.fill[Float](n)(0)
    val scores = Array.fill[Float](n, n)(0)

    for (i <- 0 until n)
      for (j <- i + 1 until n) {
        val wins = randomOthelloSeries(strategies(i), strategies(j), npairs, nrandom)._1
        val losses = 2 * npairs - wins
        scores(i)(j) += wins
        scores(j)(i) += losses
        totals(i) += wins
        totals(j) += losses
      }

    for (i <- 0 until n) {
      print(s"${names(i)}${" " * (20 - names(i).length)}" + f"${totals(i)}%4.1f:")
      for (j <- 0 until n) if (i == j) print(" ---") else print(f"${scores(i)(j)}%4.1f")
      println()
    }
  }

  /**
    * The number of moves a player has.
    */
  def mobility(player: Piece, board: Board): Int = {
    legalMoves(player, board).length
  }

  def adaptStrategy(fn: (Piece, Board) => Int): (Piece, Board) => Option[Either[Int, String]] = {
    (player: Piece, board: Board) => {
      Some(Left(fn.apply(player, board)))
    }
  }

  def main(args: Array[String]): Unit = {
    roundRobin(
      List(adaptStrategy(maximizier(countDifference)), adaptStrategy(maximizier(mobility)),
        adaptStrategy(maximizier(weightedSquares)), adaptStrategy(maximizier(modifiedWeightedSquares)), randomStrategy), 5, 10,
      List("count-difference", "mobility", "weighted", "modified-weighted", "random"))
  }
}
