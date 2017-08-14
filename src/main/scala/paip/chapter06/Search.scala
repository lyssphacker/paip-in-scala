package paip.chapter06

import paip.DebugUtils.{dbg, debug}
import paip.chapter06.Cities.{City, cityEquals}

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
    dbg("search", s"Search: $states")
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
    (x: Int) => binaryTree(x).filter((child: Int) => child <= n)
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

  def iterWideSearch[T, S](start: T,
                           isGoal: T => Boolean,
                           successors: T => List[T],
                           costFn: T => S,
                           width: Int = 1,
                           max: Int = 100): Option[T] = {
    dbg("search", s"Width $width")
    if (width <= max) {
      val result = beamSearch(start, isGoal, successors, costFn, width)
      if (result.isDefined) result
      else iterWideSearch(start, isGoal, successors, costFn, width = width + 1, max = max)
    } else None
  }

  def stateEqual[T](s1: T, s2: T): Boolean = {
    s1 match {
      case s: Int => s.equals(s2.asInstanceOf[Int])
      case s: City => s.equals(s2.asInstanceOf[City])
    }
  }

  def graphSearch[T](states: List[T],
                     isGoal: T => Boolean,
                     successors: T => List[T],
                     combiner: (List[T], List[T]) => List[T],
                     test: (T, T) => Boolean,
                     oldStates: List[T] = Nil): Option[T] = {
    dbg("search", s"Search: $states")
    if (states.isEmpty) None
    else if (isGoal.apply(states.head)) Some(states.head)
    else {
      graphSearch(
        combiner.apply(newStates(states, successors, stateEqual, oldStates), states.tail),
        isGoal,
        successors,
        combiner,
        stateEqual,
        adjoin(states.head, oldStates, stateEqual)
      )
    }
  }

  def member[T](state: T, states: List[T], test: (T, T) => Boolean): Boolean = {
    states exists ((s: T) => stateEqual(state, s))
  }

  def newStates[T](states: List[T],
                   successors: T => List[T],
                   stateEqual: (T, T) => Boolean,
                   oldStates: List[T]): List[T] = {
    successors.apply(states.head).filter((state: T) => {
      !member[T](state, states, stateEqual) && !member[T](state, oldStates, stateEqual)
    })
  }

  def adjoin[T](item: T, lst: List[T], test: (T, T) => Boolean): List[T] = {
    val result = lst.find((el: T) => {
      test.apply(el, item)
    })
    if (result.isEmpty) item :: lst
    else lst
  }

  def next2(x: Int): List[Int] = {
    List(x + 1, x + 2)
  }

  def is[T, S](value: T, key: S => T, test: (T, T) => Boolean = cityEquals _): S => Boolean = {
    (x: S) => test.apply(value, key.apply(x))
  }

  def pathSaver[T](successors: T => List[T],
                   costFn: (T, T) => Double,
                   costLeftFn: T => Double): Path[T] => List[Path[T]] = {
    (oldPath: Path[T]) => {
      val oldState = oldPath.state
      successors.apply(oldState).map((newState: T) => {
        val oldCost = oldPath.costSoFar + costFn.apply(oldState, newState)
        Path(state = newState, previous = Some(oldPath),
          costSoFar = oldCost, totalCost = oldCost + costLeftFn.apply(newState))
      })
    }
  }

  def mapPath[T](fn: T => String, path: Path[T]): List[String] = {
    if (path.previous.isEmpty) List(fn.apply(path.state))
    else fn.apply(path.state) :: mapPath(fn, path.previous.get)
  }

  case class Path[T](state: T, previous: Option[Path[T]] = None,
                     costSoFar: Double = 0.0, totalCost: Double = 0.0) {
    override def toString = s"#<Path to $state cost $totalCost>"
  }

  object Path {
    def pathTotalCost[T](p: Path[T]): Double = {
      p.totalCost
    }

    def pathState[T](path: Path[T]): T = {
      path.state
    }
  }

  def findPath[T](state: T, paths: List[Path[T]],
                  stateEqual: (T, T) => Boolean, key: Path[T] => T): Option[Path[T]] = {
    paths.find((p: Path[T]) => stateEqual.apply(key.apply(p), state))
  }

  def pathState[T](path: Path[T]): T = {
    path.state
  }

  def betterPath[T](path1: Path[T], path2: Path[T]): Boolean = {
    path1.totalCost < path2.totalCost
  }

  def insertPath[T](path: Path[T], paths: List[Path[T]]): List[Path[T]] = {
    List.concat(List(path), paths).sortWith((p1: Path[T], p2: Path[T]) => p1.totalCost < p2.totalCost)
  }

  def pathStates[T](path: Path[T]): List[T] = {
    if (path.previous.isEmpty) List(path.state)
    else path.state :: pathStates(path.previous.get)
  }

  def aStarSearch[T](paths: List[Path[T]],
                     isGoal: T => Boolean,
                     successors: T => List[T],
                     costFn: (T, T) => Double,
                     costLeftFn: T => T,
                     stateEqual: (T, T) => Boolean,
                     oldPaths: List[Path[T]]): Option[(Path[T], List[Path[T]])] = {
    dbg("search", s"Paths: $paths")
    if (paths.isEmpty) None
    else if (isGoal.apply(paths.head.state)) Some((paths.head, paths))
    else {
      val path = paths.head
      val state = path.state
      var newPaths = paths.tail
      var newOldPaths = insertPath(path, oldPaths)
      for (state2 <- successors.apply(state)) {
        val cost: Double = path.costSoFar + costFn.apply(state, state2)
        val cost2 = costLeftFn.apply(state2)
        val path2 = Path(state = state2, previous = Some(path), costSoFar = cost, totalCost = add(cost2, cost))
        var old = findPath(state2, newPaths, stateEqual, pathState[T])
        if (old.isDefined && betterPath(path2, old.get))
          newPaths = insertPath(path2, delete(old.get, newPaths))
        else {
          old = findPath(state2, newOldPaths, stateEqual, pathState[T])
          if (old.isDefined && betterPath(path2, old.get)) {
            newPaths = insertPath(path2, newPaths)
            newOldPaths = delete(old.get, newOldPaths)
          } else {
            newPaths = insertPath(path2, newPaths)
          }
        }
      }
      aStarSearch(newPaths, isGoal, successors, costFn, costLeftFn, stateEqual, newOldPaths)
    }
  }

  def add[T](a1: T, a2: Double): Double = {
    a1 match {
      case a: Int => a + a2
      case a: Double => a + a2
    }
  }

  def delete[T](item: T, lst: List[T]): List[T] = {
    lst diff List(item)
  }

  def searchAll[T, S](start: T,
                      isGoal: T => Boolean,
                      successors: T => List[T],
                      costFn: T => S,
                      beamWidth: Int): Option[List[T]] = {
    var solutions: List[T] = Nil
    beamSearch(start,
      (x: T) => {
        if (isGoal.apply(x)) solutions = x :: solutions
        false
      },
      successors, costFn, beamWidth)

    if (solutions.isEmpty) None
    else Some(solutions)
  }

  def main(args: Array[String]): Unit = {
    debug("search")
    val result = aStarSearch[Int](List(Path[Int](state = 1)), is[Int](6), next2,
      (x: Int, y: Int) => 1.0, diff(6), stateEqual[Int], Nil)
    val states = pathStates[Int](result.get._1)
    states
  }
}
