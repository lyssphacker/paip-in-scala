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

def ruleLhs(rule : (String, Any)): String = {
  rule._1
}

def ruleRhs(rule : (String, Any)): Any = {
  rule._2
}

def rewrites(key : String)(implicit grammar : List[(String, Any)]) : Option[Any] = {
  grammar.toMap.get(key)
}

def randomElt(s: Any): Any = {
  s match {
    case x : String => oneOf(x.split(" ").toList)
    case l : List[String] => l
  }
}

def oneOf(lst: List[String]): String = {
  lst.toVector(Random.nextInt(lst.size))
}


def generate(phrase : Any): List[Any] = {
  phrase match {
    case x :: xs => Intro.mappend(generate, x :: xs)
    case s : String if !rewrites(s).isEmpty =>
      generate(randomElt(rewrites(s).get))
    case _ => List(phrase)
  }
}

generate("sentence")



