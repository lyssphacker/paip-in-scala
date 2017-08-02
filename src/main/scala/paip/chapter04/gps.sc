case class Op(action: String,
              preconds: Set[String],
              addList: Set[String],
              delList: Set[String] = Set.empty);

implicit val schoolOps = List(
  Op(action = "drive-son-to-school",
    preconds = Set("son-at-home", "car-works"),
    addList = Set("son-at-school"),
    delList = Set("son-at-home")),
  Op(action = "shop-installs-battery",
    preconds = Set("car-needs-battery", "shop-knows-problem", "shop-has-money"),
    addList = Set("car-works")),
  Op(action = "tell-shop-problem",
    preconds = Set("in-communication-with-shop"),
    addList = Set("shop-knows-problem")),
  Op(action = "telephone-shop",
    preconds = Set("know-phone-number"),
    addList = Set("in-communication-with-shop")),
  Op(action = "look-up-number",
    preconds = Set("have-phone-book"),
    addList = Set("know-phone-number")),
  Op(action = "give-shop-money",
    preconds = Set("have-money"),
    addList = Set("shop-has-money"),
    delList = Set("have-money"))
)

implicit var state: Set[String] = Set.empty

def gps(stateArg: Set[String], goals: List[String]) : String = {
  state = stateArg
  if (goals.forall(achieve)) "solved"
  else "not solved"
}

def isAppropriate(goal: String, op: Op) : Boolean = {
  op.addList.contains(goal)
}

def applyOp(op: Op) : Boolean = {
  val isApplicable = op.preconds.forall(achieve)
  if (isApplicable) {
    println("executing: " + op.action)
    state = state.diff(op.delList)
    state = state.union(op.addList)
  }
  isApplicable
}

def achieve(goal: String): Boolean = {
  state.contains(goal) ||
    findAll(goal, isAppropriate).exists(applyOp)
}

def findAll(goal: String, test: (String, Op) => Boolean)
           (implicit ops: List[Op]): List[Op] = {
  for (op <- ops
    if test.apply(goal, op)
  ) yield op
}

gps(Set("son-at-home", "car-works"), List("son-at-school"))
gps(Set("son-at-home", "car-needs-battery", "have-money", "have-phone-book"), List("son-at-school"))
gps(Set("son-at-home", "car-needs-battery", "have-money"), List("son-at-school"))