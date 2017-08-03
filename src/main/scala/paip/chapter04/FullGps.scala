package paip.chapter04

import paip.chapter04.Gps.{applyOp, findAll, isAppropriate}

object FullGps {

  implicit var state: Set[String] = Set("son-at-schooll")

  def achieveAll(goals: List[String]): Boolean = {
    goals.forall(achieve) && goals.toSet.subsetOf(state)
  }

  def achieve(goal: String): Boolean = {
    state.contains(goal) ||
      findAll(goal, isAppropriate).exists(applyOp)
  }

  def main(args: Array[String]): Unit = {
    val achieved = achieveAll(List("son-at-school"))
    println(achieved)
  }
}
