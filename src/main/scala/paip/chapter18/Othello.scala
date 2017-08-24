package paip.chapter18

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

    def countDifference(p: Piece): Int = {
      count(p) - count(opponent(p))
    }

    def printBoard(): Unit = {
      println(s"${" " * 3} 1 2 3 4 5 6 7 8 [$black=${count(black)} " +
        s"$white=${count(white)} (${countDifference(black)})]")

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
      else if (aref(square).equals(opponent(player))) Some(opponent(player).id)
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

    def isValidMove(move: Int): Boolean = {
      val mod = move % 10
      move >= 11 && move <= 88 && mod >= 1 && mod <= 8
    }

    def makeMove(move: Int, player: Piece): Board = {
      aset(move, player)
      for (dir <- allDirections) makeFlips(move, player, dir)
      this
    }
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
    board.countDifference(black)
  }

  def main(args: Array[String]): Unit = {
    val board = initialBoard()
    board.printBoard()
  }
}
