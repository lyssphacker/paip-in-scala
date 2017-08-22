package paip.chapter11


import paip.chapter05.PatMatchFacility.{Bs, P}
import paip.chapter11.Unify._

import scala.util.Random

object Prolog1 {
  var dbPredicates: List[String] = List.empty

  var clausesMap: scala.collection.mutable.Map[String, List[C]] = scala.collection.mutable.Map.empty

  case class C(head: R, body: List[R]) {
    def getHeadAndBody: List[R] = head :: body
  }

  object C {
    def apply(head: R, body: R*): C = C(head, body.toList)
  }

  case class R(value: List[String]) {
    def isAtom = value.length == 1

    def first: String = value.head

    def rest: List[String] = value.tail
  }

  object R {
    def apply(s: String): R = R(s.split(" ").toList)
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
    if (clausesMap.get(pred).isEmpty)
      clausesMap.put(pred, List(clause))
    else
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

  def gensym(x: String): String = {
    x + Random.nextInt(10000)
  }

  def proveGoals(goals: R*): Unit = {
    showPrologSolutions(variablesIn(goals.toList, isVariable), proveAll(goals.toList, Bs.noBindings))
  }

  def proveAll(goals: List[R], bindings: Bs): List[Bs] = {
    if (bindings.equals(Bs.fail)) List.empty
    else if (goals.isEmpty) List(bindings)
    else prove(goals.head, bindings).flatMap((bs: Bs) => proveAll(goals.tail, bs))
  }

  def prove(goal: R, bindings: Bs): List[Bs] = {
    getClauses(predicate(goal)).get.flatMap((c: C) => {
      val newClause = renameVariables(c)
      proveAll(newClause.body, unify(P(goal.value), P(newClause.head.value), bindings))
    })
  }

  def renameVariables(clause: C): C = {
    val bindings = Bs(variablesIn(clause.getHeadAndBody, isVariable).map((v: String) => v -> gensym(v)).toMap)
    val head = clause.head.value.map((s: String) => substitute(bindings, s))
    val body = clause.body.map((r: R) => R(substitute(bindings, r.value.mkString(" "))))
    C(R(head), body)
  }

  def variablesIn(goals: List[R], predicate: String => Boolean): List[String] = {
    goals.flatMap((g: R) => uniqueFindAnywhereif(predicate, g))
  }

  def uniqueFindAnywhereif(predicate: String => Boolean, goal: R, foundSoFar: List[String] = List.empty): List[String] = {
    if (goal.isAtom) {
      if (predicate.apply(goal.first)) adjoin(goal.first, foundSoFar)
      else foundSoFar
    } else {
      uniqueFindAnywhereif(predicate, R(goal.first), uniqueFindAnywhereif(predicate, R(goal.rest), foundSoFar))
    }
  }

  def adjoin(item: String, lst: List[String]): List[String] = {
    if (lst.contains(item)) lst
    else item :: lst
  }

  def showPrologSolutions(vars: List[String], bindings: List[Bs]): Unit = {
    if (bindings.isEmpty) println("No.")
    else bindings.foreach((b: Bs) => showPrologVars(vars, b))
  }

  def showPrologVars(vars: List[String], bindings: Bs): Unit = {
    if (vars.isEmpty) print("Yes")
    else vars.foreach((v: String) => {
      val subst = substBindings(bindings, P(v))
      if (subst.isDefined) println(s"$v = ${subst.get}")
    })
  }

  def main(args: Array[String]): Unit = {
    addClauses(C(R("likes Kim Robin")))
    addClauses(C(R("likes Sandy Lee")))
    addClauses(C(R("likes Sandy Kim")))
    addClauses(C(R("likes Robin cats")))
    addClauses(C(R("likes Sandy ?x"), R("likes ?x cats")))
    addClauses(C(R("likes Sandy ?x"), R("likes ?x cats"), R("likes ?x Kim")))
    addClauses(C(R("likes ?x ?x")))

//    proveGoals(R("likes Sandy ?who"))
    proveGoals(R("likes ?who Sandy"))
//    proveGoals(R("likes Robin Lee"))
//    proveGoals(R("likes ?x ?y"), R("likes ?y ?x"))
  }
}
