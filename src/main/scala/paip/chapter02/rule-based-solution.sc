
import paip.chapter01.Chapter01
import paip.chapter02.Chapter02
import paip.chapter02.Chapter02.{Concat, OneOf, Rhs}

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

def generate(phrase: Any): List[Any] = {
  phrase match {
    case lst: List[Any] => Chapter01.mappend(generate, lst)
    case s: String =>
      Chapter02.rewrites(s) match {
        case some: Some[Rhs] => generate(some.get)
        case None => List(phrase)
      }
    case OneOf(value) => generate(Chapter02.randomElt(value))
    case Concat(value) => generate(value)
    case _ => List(phrase)
  }
}

def generateTree(phrase: Any): List[Any] = {
  phrase match {
    case lst: List[Any] => Chapter02.mapcar(generateTree, lst)
    case s: String =>
      Chapter02.rewrites(s) match {
        case some: Some[Rhs] => s :: generateTree(some.get)
        case None => List(phrase)
      }
    case Concat(value) => generateTree(value)
    case OneOf(value) => generateTree(Chapter02.randomElt(value))
    case _ => List(phrase)
  }
}





