package paip.chapter04

import paip.chapter04.Gps.Op
import paip.utils.DebugUtils._

object FullGps {

//  implicit val convertedSchoolOps = Gps.schoolOps.map(convertOp)

  implicit val convertedNewSchoolOps = (Op(action = "taxi-son-to-school",
    preconds = Set("son-at-home", "have-money"),
    addList = Set("son-at-school"),
    delList = Set("son-at-home", "have-money")) :: Gps.schoolOps).map(convertOp)

  //  implicit val convertedBananaOps = MonkeyAndBanans.bananaOps.map(convertOp)

  //  implicit val convertedMazeOps = List((1, 2), (2, 3), (3, 4), (4, 9), (9, 14), (9, 8), (8, 7), (7, 12), (12, 13),
  //    (12, 11), (11, 6), (11, 16), (16, 17), (17, 22), (21, 22), (22, 23),
  //    (23, 18), (23, 24), (24, 19), (19, 20), (20, 15), (15, 10), (10, 5), (20, 25)).flatMap(makeMazeOps).map(convertOp)

  //  implicit val convertedBlocksWorldOps = makeBlockOps(List("a", "b")).map(convertOp)
//  implicit val convertedBlocksWorldOps = makeBlockOps(List("a", "b", "c")).map(convertOp)

  def gps(state: List[String], goals: List[String]): List[String] = {
    val currentState = achieveAll("start" :: state, goals, Nil)
    if (currentState.isDefined)
      currentState.get.filter(isAction)
    else Nil
  }

  def isAction(s: String): Boolean = {
    s.equals("start") || s.startsWith("executing")
  }

  def achieveEach(state: List[String], goals: List[String],
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

  def achieveAll(state: List[String], goals: List[String],
                 goalStack: List[String]): Option[List[String]] = {
    var result: Option[List[String]] = None
    orderings(goals).find((ordering: List[String]) => {
      result = achieveEach(state, ordering, goalStack)
      result.isDefined
    })
    result
  }

  def orderings(l: List[String]): List[List[String]] = {
    if (l.length > 1) List(l, l.reverse)
    else List(l)
  }

  def achieve(state: List[String], goal: String,
              goalStack: List[String]): Option[List[String]] = {
    dbgIndent("gps", goalStack.size, s"Goal: $goal")
    if (state.contains(goal))
      Some(state)
    else if (goalStack.contains(goal))
      None
    else {
      val ops = appropriateOps(goal, state)
      var result: Option[List[String]] = None
      ops.find((op: Op) => {
        result = applyOp(state, goal, op, goalStack)
        result.isDefined
      })
      result
    }
  }

  def appropriateOps(goal: String, state: List[String]): List[Op] = {
    val ops = FullGps.findAll(goal, FullGps.isAppropriate)
    val fn = (s: String) => !state.contains(s)
    ops.sortWith((op1, op2) => {
      countIf(fn, op1.preconds.toList) <
        countIf(fn, op2.preconds.toList)
    })
  }

  def countIf(fn: String => Boolean, preconds: List[String]): Int = {
    preconds.count(fn)
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
    dbgIndent("gps", goalStack.size, s"Consider: ${op.action}")
    val state2 = achieveAll(state, op.preconds.toList, goal :: goalStack)
    if (state2.isDefined) {
      dbgIndent("gps", goalStack.size, s"Action: ${op.action}")
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
    // original domain
//        val result = gps(List("son-at-home", "car-needs-battery",
//          "have-money", "have-phone-book"),
//          List("son-at-school"))

    // monkey and bananas domain
    //    val result = gps(List("at-door", "on-floor",
    //      "has-ball", "hungry", "chair-at-door"),
    //      List("not-hungry"))

    // maze domain
    //    val result = gps(List("at 1"),
    //      List("at 25"))
    //    result

    // blocks world domain
    val result = gps(List("son-at-home", "have-money", "car-works"),
      List("son-at-school", "have-money"))
    result
  }
}
