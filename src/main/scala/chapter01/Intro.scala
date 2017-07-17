package chapter01

/**
  * Created by lixkid on 01.01.17..
  */
object Intro {
  /**
    * "A list of titles that can appear at the start of a name."
    */
  private final val Titles = List("Mr", "Mrs", "Miss", "Ms", "Sir", "Madam", "Dr", "Admiral", "Major", "General")

  /**
    * Select the last name from a name represented as a list.
    *
    * @param name Name.
    * @return Last name.
    */
  def lastName(name: List[String]): String = name.last

  /**
    * Select the first name from a name represented as a list.
    *
    * @param name Name.
    * @return First name.
    */
  def firstName(name: List[String]): String = {
    if (Titles.contains(name.head)) firstName(name.drop(1)) else name.head
  }

  /**
    * Append the results of calling fn on each element of list.
    *
    * @param fn  Function to call on each element of the list.
    * @param lst List on which to apply function fn.
    * @return List which is the result of appending the results
    *         of calling function fn on each element of list.
    */
  def mappend(fn: Any => List[Any], lst: List[Any]): List[Any] = {
    lst.flatMap(fn)
  }

  /**
    * If x is a number, return a list of x and -x.
    * If x is not a number, return empty list.
    * @param x
    * @return
    */
  def numberAndNegation(x: Any): List[Any] = {
    val result = x match {
      case a: Int => List(a, -a)
      case _ => List.empty
    }

    return result
  }

  /**
    * Given a list, return only the numbers and their negations.
    * @param lst
    * @return List of numbers and their negations.
    */
  def numbersAndNegations(lst: List[Any]): List[Any] = {
    mappend(numberAndNegation, lst)
  }
}
