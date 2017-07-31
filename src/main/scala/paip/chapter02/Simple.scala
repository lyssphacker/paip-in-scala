package paip.chapter02

import scala.util.Random

object Simple {
  def randomElt(set: Set[String]): String = {
    oneOf(set)
  }

  def oneOf(set: Set[String]): String = {
    set.toVector(Random.nextInt(set.size))
  }

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
    (article() +: adjOrNone()) ::: (noun() +: ppOrNone())
  }

  def prep(): String = {
    oneOf(Set("to", "in", "by", "with", "on"))
  }

  def adj(): String = {
    oneOf(Set("big", "little", "blue", "green", "adiabatic"))
  }

  def pp(): List[String] = {
    prep() +: nounPhrase()
  }

  def adjOrNone(): List[String] = {
    if (Random.nextInt(2) == 0)
      List.empty[String]
    else
      adj() +: adjOrNone()
  }

  def ppOrNone(): List[String] = {
    if (Random.nextInt(2) == 0)
      List.empty[String]
    else
      pp() ::: ppOrNone()
  }
}
