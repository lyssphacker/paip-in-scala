package paip.chapter18

object Othello {

  object Piece extends Enumeration {
    type Piece = Value
    val empty = Value(".")
    val black = Value("@")
    val white = Value("?")
    val outer = Value("?")
  }

  val allDirections = List(-11, -10, -9, -1, 1, 9, 10, 11)

  import paip.chapter18.Othello.Piece._

  def opponent(player: Piece): Piece = {
    if (player.equals(black)) white
    else black
  }

  case class Board() {
    val arr: Array[Piece] = new Array(100)

    def aref(square: Int): Piece = {
      arr(square)
    }

    def aset(square: Int, value: Piece): Unit = {
      arr(square) = value
    }

  }

  def main(args: Array[String]): Unit = {
    val board = Board()
    board.arr
  }
}
