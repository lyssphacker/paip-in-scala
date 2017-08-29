package paip.chapter18

import paip.chapter18.Othello.weights

object Othello2 {
  val allSquares: List[Int] = (11 to 88).filter((i: Int) => {
    val mod = i % 10
    mod >= 1 && mod <= 8
  }).toList.sortWith(weights(_) > weights(_))
}
