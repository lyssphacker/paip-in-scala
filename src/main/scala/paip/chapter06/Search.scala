package paip.chapter06

import paip.DebugUtils.dbg

object Search {
  def binaryTree(x: Int): List[Int] = {
    List(2 * x, 1 + 2 * x)
  }

  def is(value: Int): Int => Boolean = {
    (x: Int) => x.equals(value)
  }

  def treeSearch(states: List[Int],
                 isGoal: Int => Boolean,
                 successors: Int => List[Int],
                 combiner: (List[Int], List[Int]) => List[Int]): Option[Int] = {
    dbg("search", "Search: " + states)
    if (states.isEmpty) None
    else if (isGoal.apply(states.head)) Some(states.head)
    else treeSearch(
      combiner.apply(successors.apply(states.head), states.tail),
      isGoal,
      successors,
      combiner)
  }

  def append(lst1: List[Int], lst2: List[Int]): List[Int] = {
    lst1 ::: lst2
  }

  def depthFirstSearch(start: Int,
                       isGoal: Int => Boolean,
                       successors: Int => List[Int]): Option[Int] = {
    treeSearch(List(start), isGoal, successors, append)
  }

  def prepend(lst1: List[Int], lst2: List[Int]): List[Int] = {
    append(lst2, lst1)
  }

  def breadthFirstSearch(start: Int,
                         isGoal: Int => Boolean,
                         successors: Int => List[Int]): Option[Int] = {
    treeSearch(List(start), isGoal, successors, prepend)
  }

  def finiteBinaryTree(n: Int): Int => List[Int] = {
    (x: Int) => binaryTree(x).filter((child: Int) => child < n)
  }

  def diff(num: Int): Int => Int = {
    (x: Int) => Math.abs(x - num)
  }

  def sorter(costFn: Int => Int): (List[Int], List[Int]) => List[Int] = {
    (n: List[Int], o: List[Int]) =>
      append(n, o).sortWith(costFn(_) < costFn(_))
  }

  def bestFirstSearch(start: Int,
                      isGoal: Int => Boolean,
                      successors: Int => List[Int],
                      costFn: Int => Int): Option[Int] = {
    treeSearch(List(start), isGoal, successors, sorter(costFn))
  }

  def priceIsRight(price: Int): Int => Int = {
    (x: Int) =>
      if (x > price) Int.MaxValue
      else price - x
  }

  def beamSearch(start: Int,
                 isGoal: Int => Boolean,
                 successors: Int => List[Int],
                 costFn: Int => Int,
                 beamWidth: Int): Option[Int] = {
    treeSearch(List(start), isGoal, successors,
      (o: List[Int], n: List[Int]) => {
        val sorted = sorter(costFn).apply(o, n)
        if (beamWidth > sorted.length) sorted
        else sorted.slice(0, beamWidth)
      })
  }
}
