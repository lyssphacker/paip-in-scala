package paip.chapter02

import scala.util.Random

object Utils02 {
  abstract class Rhs

  case class Concat(lsts: List[String]) extends Rhs

  object Concat {
    def apply(lst: String*) = new Concat(lst.toList)
  }

  case class OneOf(lsts: List[Any]) extends Rhs

  object OneOf {
    def apply(lst: Any*) = new OneOf(lst.toList)
  }

  def mapcar(fn: Any => Any, lst: List[Any]): List[Any] = {
    lst.map(fn)
  }

  def rewrites(key: String)(implicit grammar: List[(String, Rhs)]): Option[Rhs] = {
    grammar.toMap.get(key)
  }

  def randomElt(lst: List[Any]): Any = {
    lst.toVector(Random.nextInt(lst.size))
  }
}
