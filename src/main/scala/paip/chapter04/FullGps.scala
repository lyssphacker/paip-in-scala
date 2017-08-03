package paip.chapter04

import paip.chapter04.Gps.Op

object FullGps {

  implicit val convertedSchoolOps = Gps.schoolOps.map(convertOp)

  implicit var state: Set[String] = Set.empty

  def applyOp(op: Op): Boolean = {
    val isApplicable = op.preconds.forall(achieve)
    if (isApplicable) {
      println("executing: " + op.action)
      state = state.diff(op.delList)
      state = state.union(op.addList)
    }
    isApplicable
  }

  def findAll(goal: String, test: (String, Op) => Boolean)
             (implicit ops: List[Op]): List[Op] = {
    for (op <- ops
         if test.apply(goal, op)
    ) yield op
  }

  def isAppropriate(goal: String, op: Op): Boolean = {
    op.addList.contains(goal)
  }

  def achieveAll(goals: List[String]): Boolean = {
    goals.forall(achieve) && goals.toSet.subsetOf(state)
  }

  def achieve(goal: String): Boolean = {
    state.contains(goal) ||
      findAll(goal, isAppropriate).exists(applyOp)
  }

  def isExecuting(action: String): Boolean = {
    action.startsWith("executing")
  }

  def convertOp(op: Op): Op = {
    if (!op.addList.exists(isExecuting)) {
      val action = op.action
      val el = s"executing $action"
      op.addList += el
    }
    op
  }

  def gps(stateArg: Set[String], goals: List[String]): String = {
    state = stateArg
    if (goals.forall(achieve)) "solved"
    else "not solved"
  }

  def main(args: Array[String]): Unit = {
    convertedSchoolOps
    println()
  }
}
