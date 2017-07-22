import chapter01.Intro

import scala.util.Random

implicit val simpleGrammar = List(
  "sentence" -> List("noun-phrase", "verb-phrase"),
  "noun-phrase" -> List("Article", "Noun"),
  "verb-phrase" -> List("Verb", "noun-phrase"),
  "Article" -> "the a",
  "Noun" -> "man ball woman table",
  "Verb" -> "hit took saw liked"
)

def ruleLhs(rule : (String, AnyRef)): String = {
  rule._1
}

def ruleRhs(rule : (String, AnyRef)): AnyRef = {
  rule._2
}

def rewrites(key : Any)(implicit grammar : List[(Any, AnyRef)]) : Option[AnyRef] = {
  grammar.toMap.get(key)
}

def randomElt(s: AnyRef): String = {
  s match {
    case x : String => oneOf(x.split(" ").toList)
    case l : List[String] => oneOf(l)
    case _ => null
  }
}

def oneOf(lst: List[String]): String = {
  lst.toVector(Random.nextInt(lst.size))
}


def generate(phrase : Any): List[Any] = {
  phrase match {
    case x :: xs => Intro.mappend(generate, x :: xs)
    case s : String if !rewrites(s).isEmpty =>
      generate(randomElt(rewrites(phrase).get))
    case _ => List(phrase)
  }
}

randomElt("hit took saw liked")
rewrites("sentence")

randomElt(rewrites("sentence").get)
generate(List("noun-phrase", "verb-phrase"))
generate(List("Article", "Noun"))
generate(List("Verb", "noun-phrase"))
generate("noun-phrase")



