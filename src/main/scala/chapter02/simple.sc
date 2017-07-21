import scala.util.Random

def randomElt(set: Set[String]): String = {
  oneOf(set)
}

def oneOf(set: Set[String]): String = {
  set.toVector(Random.nextInt(set.size))
}

randomElt(Set("the", "a", "not"))

def sentence(): List[String] = {
  nounPhrase() ::: verbPhrase()
}

def verb(): String = {
  oneOf(Set("hit", "took", "saw", "liked"))
}

def noun(): String = {
  oneOf(Set("man", "ball", "woman", "table"))
}

def article(): String = {
  oneOf(Set("the", "a"))
}

def verbPhrase(): List[String] = {
  verb() +: nounPhrase()
}

def nounPhrase(): List[String] = {
  List(article(), noun())
}

sentence()