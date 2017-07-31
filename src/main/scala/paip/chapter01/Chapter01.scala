package paip.chapter01

object Chapter01 {
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
}
