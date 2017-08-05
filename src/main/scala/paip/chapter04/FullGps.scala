package paip.chapter04

import paip.chapter04.Gps.Op
import paip.chapter04.Utils04._

object FullGps {

  implicit val convertedSchoolOps = Gps.schoolOps.map(convertOp)

//  implicit val convertedBananaOps = MonkeyAndBanans.bananaOps.map(convertOp)

  def gps(state: List[String], goals: List[String]): List[String] = {
    val currentState = achieveAll("start" :: state, goals, Nil)
    if (currentState.isDefined)
      currentState.get.filter((s: String) => {
        s.equals("start") || s.startsWith("executing")
      })
    else Nil
  }

  def achieveAll(state: List[String], goals: List[String],
                 goalStack: List[String]): Option[List[String]] = {
    var currentState: Option[List[String]] = Some(state)
    val isCurrentStateDefined = goals.forall((g: String) => {
      currentState = achieve(currentState.get, g, goalStack)
      currentState.isDefined
    })
    if (isCurrentStateDefined && goals.forall(currentState.get.contains))
      currentState
    else
      None
  }

  def achieve(state: List[String], goal: String,
              goalStack: List[String]): Option[List[String]] = {
    dbgIndent("gps", goalStack.size, s"Goal: $goal")
    if (state.contains(goal))
      Some(state)
    else if (goalStack.contains(goal))
      None
    else {
      val ops = findAll(goal, isAppropriate)
      var result: Option[List[String]] = None
      ops.find((op: Op) => {
        result = applyOp(state, goal, op, goalStack)
        result.isDefined
      })
      result
    }
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

  def applyOp(state: List[String], goal: String, op: Op,
              goalStack: List[String]): Option[List[String]] = {
    dbgIndent("gps", goalStack.size, "Consider: " + op.action)
    val state2 = achieveAll(state, op.preconds.toList, goal :: goalStack)
    if (state2.isDefined) {
      dbgIndent("gps", goalStack.size, "Action: " + op.action)
      val state2Filtered = state2.get.filter(!op.delList.contains(_))
      Some(state2Filtered ::: op.addList.toList)
    } else {
      None
    }
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

  def main(args: Array[String]): Unit = {
    debug("gps")
    val result = gps(List("son-at-home", "car-needs-battery",
      "have-money", "have-phone-book"),
      List("son-at-school"))

//    val result = gps(List("at-door", "on-floor",
//      "has-ball", "hungry", "chair-at-door"),
//      List("not-hungry"))
//    result
  }
}
