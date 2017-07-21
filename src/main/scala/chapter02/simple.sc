import scala.util.Random

def randomElt(set: Set[String]): String = {
  oneOf(set)
}

def oneOf(set: Set[String]): String = {
  set.toVector(Random.nextInt(set.size))
}

randomElt(Set("the", "a", "not"))

def sentence(): Unit = {
  val l = List(randomElt(Set("the", "a", "not")), randomElt(Set("the", "a", "not")))
  println(l)
}

sentence()