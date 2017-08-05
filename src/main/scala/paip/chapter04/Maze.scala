package paip.chapter04

import paip.chapter04.FullGps.gps
import paip.chapter04.Gps.Op

object Maze {
  def makeMazeOp(here: String, there: String): Op = {
    Op(action = s"move from $here to $there",
      preconds = Set(s"at $here"),
      addList = Set(s"at $there"),
      delList = Set(s"at $here"))
  }

  def makeMazeOps(pair: (Int, Int)): List[Op] = {
    List(makeMazeOp(pair._1.toString, pair._2.toString),
      makeMazeOp(pair._2.toString, pair._1.toString))
  }

  def findPath(start: String, end: String): List[String] = {
    val results = gps(List(s"at $start"), List(s"at $end"))
    if (!results.isEmpty) {
      start :: results
        .filter((s: String) => !s.equals("start"))
        .map(destination)
    }
    else Nil
  }

  def destination(action: String): String = {
    action.split(" ")(5)
  }
}
