import chapter01.Intro

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

def combineAll(xlist : List[Expansion], ylist : List[Expansion]) : List[Any] = {
  Intro.mappend((y: Any) =>
    mapcar((x: Any) => {
      val Expansion(xval) = x
      val Expansion(yval) = y
      xval ::: yval
    }, xlist), ylist)
}

combineAll(List(Expansion("a"), Expansion("b")), List(Expansion("1"), Expansion("2")))
