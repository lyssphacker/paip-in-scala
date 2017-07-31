import paip.chapter01.Chapter01

/**
  * "A list of titles that can appear at the start of a name."
  */
val Titles = List("Mr", "Mrs", "Miss", "Ms", "Sir", "Madam", "Dr", "Admiral", "Major", "General")

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
  Chapter01.mappend(numberAndNegation, lst)
}