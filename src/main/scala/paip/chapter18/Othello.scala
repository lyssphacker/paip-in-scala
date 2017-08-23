package paip.chapter18

object Othello {

  object Piece extends Enumeration {
    type Piece = Value
    val empty = Value(".")
    val black = Value("@")
    val white = Value("?")
    val outer = Value("?")
  }

}
