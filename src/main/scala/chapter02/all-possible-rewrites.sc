import chapter01.Intro

import scala.util.Random

abstract class Rhs

case class Concat(lsts: List[String]) extends Rhs

object Concat {
  def apply(lst: String*) = new Concat(lst.toList)
}

case class OneOf(lsts: List[Any]) extends Rhs

object OneOf {
  def apply(lst: Any*) = new OneOf(lst.toList)
}

implicit val simpleGrammar = List(
  "sentence" -> Concat("noun-phrase", "verb-phrase"),
  "noun-phrase" -> Concat("Article", "Noun"),
  "verb-phrase" -> Concat("Verb", "noun-phrase"),
  "Article" -> OneOf("the", "a"),
  "Noun" -> OneOf("man", "ball", "woman", "table"),
  "Verb" -> OneOf("hit", "took", "saw", "liked")
)

def randomElt(lst: List[Any]): Any = {
  lst.toVector(Random.nextInt(lst.size))
}

def rewrites(key: String)(implicit grammar: List[(String, Rhs)]): Option[Rhs] = {
  grammar.toMap.get(key)
}

def mapcar(fn: Any => Any, lst: List[Any]): List[Any] = {
  lst.map(fn)
}

object Expansion {
  def apply(values: Any*) = new Expansion(values.toList)
}

case class Expansion(values: List[Any])

def combineAll(xlist : List[Any], ylist : List[Any]) : List[Any] = {
  println("combineAll xlist: " + xlist)
  println("combineAll ylist: " + ylist)
  Intro.mappend((y: Any) =>
    mapcar((x: Any) => {
      val Expansion(xval) = x
      val Expansion(yval) = y
      xval ::: yval
    }, xlist), ylist)
}

def generateAll(phrase: Any): List[Any] = {
  println("generateAll: " + phrase)
  phrase match {
    case Nil => List(Expansion())
    case lst: List[Any] =>
      combineAll(generateAll(lst.head), generateAll(lst.tail))
    case s: String =>
      rewrites(s) match {
        case some: Some[Rhs] => generateAll(some.get)
        case None => List(Expansion(phrase))
      }
    case OneOf(value) => Intro.mappend(generateAll, value)
    case Concat(value) => generateAll(value)
  }
}

//combineAll(List(Expansion("a"), Expansion("b")), List(Expansion("1"), Expansion("2")))

generateAll("noun-phrase")

//combineAll(List(Expansion("the"), Expansion("a")),
//  List(Expansion("man"), Expansion("ball"), Expansion("woman"), Expansion("table")))
