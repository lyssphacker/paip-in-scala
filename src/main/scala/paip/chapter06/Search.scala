package paip.chapter06

import paip.DebugUtils.{dbg, debug}

object Search {
  def binaryTree(x: Int): List[Int] = {
    List(2 * x, 1 + 2 * x)
  }

  def is[T](value: T): T => Boolean = {
    (x: T) => x.equals(value)
  }

  def treeSearch[T](states: List[T],
                    isGoal: T => Boolean,
                    successors: T => List[T],
                    combiner: (List[T], List[T]) => List[T]): Option[T] = {
    dbg("search", "Search: " + states)
    if (states.isEmpty) None
    else if (isGoal.apply(states.head)) Some(states.head)
    else treeSearch(
      combiner.apply(successors.apply(states.head), states.tail),
      isGoal,
      successors,
      combiner)
  }

  def append[T](lst1: List[T], lst2: List[T]): List[T] = {
    lst1 ::: lst2
  }

  def depthFirstSearch[T](start: T,
                          isGoal: T => Boolean,
                          successors: T => List[T]): Option[T] = {
    treeSearch(List(start), isGoal, successors, append[T])
  }

  def prepend[T](lst1: List[T], lst2: List[T]): List[T] = {
    append(lst2, lst1)
  }

  def breadthFirstSearch[T](start: T,
                            isGoal: T => Boolean,
                            successors: T => List[T]): Option[T] = {
    treeSearch(List(start), isGoal, successors, prepend[T])
  }

  def finiteBinaryTree(n: Int): Int => List[Int] = {
    (x: Int) => binaryTree(x).filter((child: Int) => child < n)
  }

  def diff(num: Int): Int => Int = {
    (x: Int) => Math.abs(x - num)
  }

  def lessThan[S](first: S, second: S): Boolean = {
    first match {
      case i: Int => i < second.asInstanceOf[Int]
      case d: Double => d < second.asInstanceOf[Double]
    }
  }

  def sorter[T, S](costFn: T => S): (List[T], List[T]) => List[T] = {
    (n: List[T], o: List[T]) =>
      append(n, o).sortWith((e1: T, e2: T) => lessThan[S](costFn(e1), costFn(e2)))
  }

  def bestFirstSearch[T](start: T,
                         isGoal: T => Boolean,
                         successors: T => List[T],
                         costFn: T => Int): Option[T] = {
    treeSearch(List(start), isGoal, successors, sorter(costFn))
  }

  def priceIsRight(price: Int): Int => Int = {
    (x: Int) =>
      if (x > price) Int.MaxValue
      else price - x
  }

  def beamSearch[T, S](start: T,
                    isGoal: T => Boolean,
                    successors: T => List[T],
                    costFn: T => S,
                    beamWidth: Int): Option[T] = {
    treeSearch(List(start), isGoal, successors,
      (o: List[T], n: List[T]) => {
        val sorted = sorter(costFn).apply(o, n)
        if (beamWidth > sorted.length) sorted
        else sorted.slice(0, beamWidth)
      })
  }

  def main(args: Array[String]): Unit = {
    debug("search")
    beamSearch[Int, Int](1, is(12), binaryTree, priceIsRight(12), 2)
  }
}
