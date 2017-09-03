package paip.chapter02

import paip.chapter01.Utils01.mappend
import paip.chapter02.Utils02._

object AllPossibleRewrites {
  implicit val simpleGrammar = List(
    "sentence" -> Concat("noun-phrase", "verb-phrase"),
    "noun-phrase" -> Concat("Article", "Noun"),
    "verb-phrase" -> Concat("Verb", "noun-phrase"),
    "Article" -> OneOf("the", "a"),
    "Noun" -> OneOf("man", "ball", "woman", "table"),
    "Verb" -> OneOf("hit", "took", "saw", "liked")
  )

  object Expansion {
    def apply(values: Any*) = new Expansion(values.toList)
  }

  case class Expansion(values: List[Any])

  def combineAll[T](xlist: List[T], ylist: List[T]): List[Expansion] = {
    mappend[T, Expansion]((y: T) =>
      mapcar[T, Expansion]((x: T) => {
        val Expansion(xval) = x
        val Expansion(yval) = y
        Expansion(xval ::: yval)
      }, xlist), ylist)
  }

  def mapcar[T, S](fn: T => S, lst: List[T]): List[S] = {
    lst.map(fn)
  }

  def mappend[T, S](fn: T => List[S], lst: List[T]): List[S] = {
    lst.flatMap(fn)
  }

  def generateAll(phrase: Any): List[Any] = {
    phrase match {
      case Nil => List(Expansion())
      case lst: List[Any] =>
        combineAll(generateAll(lst.head), generateAll(lst.tail))
      case s: String =>
        rewrites(s) match {
          case some: Some[Rhs] =>
            some.get match {
              case OneOf(value) => mappend(generateAll, value)
              case Concat(value) => generateAll(value)
            }
          case None => List(Expansion(phrase))
        }
    }
  }
}
