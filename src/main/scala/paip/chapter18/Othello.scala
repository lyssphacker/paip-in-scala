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
    val board: Board = Board(new Array[Piece](100))
    board.aset(2, white)
    val board1 = board.copy()
    board1
  }
}
