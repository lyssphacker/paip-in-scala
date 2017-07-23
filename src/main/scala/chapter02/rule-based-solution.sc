import chapter01.Intro

import scala.util.Random

abstract class Rhs

case class Concat(lsts: List[String]) extends Rhs
object Concat {
  def apply(lst: String*) = new Concat(lst.toList)
}

case class OneOf(lsts: List[String]) extends Rhs
object OneOf {
  def apply(lst: String*) = new OneOf(lst.toList)
}

implicit val simpleGrammar = List(
  "sentence" -> Concat("noun-phrase", "verb-phrase"),
  "noun-phrase" -> Concat("Article", "Noun"),
  "verb-phrase" -> Concat("Verb", "noun-phrase"),
  "Article" -> OneOf("the", "a"),
  "Noun" -> OneOf("man", "ball", "woman", "table"),
  "Verb" -> OneOf("hit", "took", "saw", "liked")
)

def ruleLhs(rule : (String, Rhs)): String = {
  rule._1
}

def ruleRhs(rule : (String, Rhs)): Rhs = {
  rule._2
}

def rewrites(key : String)(implicit grammar : List[(String, Rhs)]) : Option[Rhs] = {
  grammar.toMap.get(key)
}

def randomElt(lst: List[String]): String = {
  lst.toVector(Random.nextInt(lst.size))
}

def generate(phrase : Any): List[Any] = {
  phrase match {
    case x :: xs => Intro.mappend(generate, x :: xs)
    case s : String if rewrites(s).isDefined =>
      val rewrite = rewrites(s).get
      rewrite match {
        case OneOf(value) => generate(randomElt(value))
        case Concat(value) => generate(value)
      }

    case _ => List(phrase)
  }
}

generate("sentence")



