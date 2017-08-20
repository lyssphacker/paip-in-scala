package paip.chapter11

object Prolog1 {
  var dbPredicates: List[String] = List.empty

  var clausesMap: scala.collection.mutable.Map[String, List[C]] = scala.collection.mutable.Map.empty

  case class C(head: R, body: Option[R]) {
    def isAtom() = body.isDefined && body.get.value.nonEmpty
  }

  case class R(value: List[String])

  object R {
    def apply(s: String) = R(s.split(" ").toList)
  }

  def addClauses(clauses: C*): Unit = {
    for (clause <- clauses) addClause(clause)
  }

  def predicate(relation: R): String = {
    relation.value.head
  }

  def getClauses(pred: String): Option[List[C]] = {
    clausesMap.get(pred)
  }

  def isVariable(s: String): Boolean = {
    s.startsWith("?")
  }

  def pushNewPredicate(pred: String): Unit = {
    if (!dbPredicates.contains(pred))
      dbPredicates = pred :: dbPredicates
  }

  def addToClauseMap(pred: String, clause: C): Unit = {
    clausesMap(pred) = clause :: clausesMap(pred)
  }

  def addClause(clause: C): Unit = {
    val pred = predicate(clause.head)
    assert(!isVariable(pred))
    pushNewPredicate(pred)
    addToClauseMap(pred, clause)
  }

  def clearDb(): Unit = {
    clausesMap.foreach(e => if (dbPredicates.contains(e._1)) clausesMap.remove(e._1))
  }
}
