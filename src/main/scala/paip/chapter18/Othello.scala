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

  def main(args: Array[String]): Unit = {
    val board = initialBoard()
    board.printBoard()
  }
}
