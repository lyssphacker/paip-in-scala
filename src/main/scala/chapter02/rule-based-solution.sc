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

implicit val biggerGrammar = List(
  "sentence" -> Concat("noun-phrase", "verb-phrase"),
  "noun-phrase" -> OneOf(Concat("Article", "Adj*", "Noun", "PP*"), "Name", "Pronoun"),
  "verb-phrase" -> Concat("Verb", "noun-phrase", "PP*"),
  "PP*" -> OneOf(Nil, Concat("PP", "PP*")),
  "Adj*" -> OneOf(Nil, Concat("Adj", "Adj*")),
  "PP" -> Concat("Prep", "noun-phrase"),
  "Prep" -> OneOf("to", "in", "by", "on"),
  "Adj" -> OneOf("big", "little", "blue", "green", "adiabatic"),
  "Article" -> OneOf("the", "a"),
  "Name" -> OneOf("Pat", "Kim", "Lee", "Terry", "Robin"),
  "Noun" -> OneOf("man", "ball", "woman", "table"),
  "Verb" -> OneOf("hit", "took", "saw", "liked"),
  "Pronoun" -> OneOf("he", "she", "it", "these", "that")
)

def ruleLhs(rule: (String, Rhs)): String = {
  rule._1
}

def ruleRhs(rule: (String, Rhs)): Rhs = {
  rule._2
}

def rewrites(key: String)(implicit grammar: List[(String, Rhs)]): Option[Rhs] = {
  grammar.toMap.get(key)
}

def randomElt(lst: List[Any]): Any = {
  lst.toVector(Random.nextInt(lst.size))
}

def generate(phrase: Any): List[Any] = {
  phrase match {
    case lst: List[Any] => Intro.mappend(generate, lst)
    case s: String =>
      rewrites(s) match {
        case some: Some[Rhs] => generate(some.get)
        case None => List(phrase)
      }
    case OneOf(value) => generate(randomElt(value))
    case Concat(value) => generate(value)
    case _ => List(phrase)
  }
}

def generateTree(phrase: Any): List[Any] = {
  phrase match {
    case lst: List[Any] => mapcar(generateTree, lst)
    case s: String =>
      rewrites(s) match {
        case some: Some[Rhs] => s :: generateTree(some.get)
        case None => List(phrase)
      }
    case Concat(value) => generateTree(value)
    case OneOf(value) => generateTree(randomElt(value))
    case _ => List(phrase)
  }
}

def mapcar(fn: Any => Any, lst: List[Any]): List[Any] = {
  lst.map(fn)
}

//generate("sentence")
generateTree("sentence")





