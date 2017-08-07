package paip.chapter05

import scala.util.Random

object Utils05 {
  def randomElt[T](lst: List[T]): T = {
    lst.toVector(Random.nextInt(lst.size))
  }
}
