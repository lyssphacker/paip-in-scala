package paip.chapter18

object Othello {
  object Piece extends Enumeration {
    type Piece = Value
    val empty, black, white, outer = Value
  }
}
