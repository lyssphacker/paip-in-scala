package paip.chapter11


import paip.chapter05.PatMatchFacility.{Bs, P}
import paip.chapter11.Unify._

import scala.util.Random

object Prolog1 {
  var dbPredicates: List[String] = List.empty

  var clausesMap: scala.collection.mutable.Map[String, List[C]] = scala.collection.mutable.Map.empty

  case class C(head: R, body: Option[R])

  case class R(value: List[String]) {
    def isAtom = value.nonEmpty

    def first: String = value.head

    def rest: List[String] = value.tail
  }

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

  def gensym(x: String): String = {
    x + Random.nextInt(10000)
  }

  def proveGoals(goals: R*): Unit = {
    showPrologSolutions(variablesIn(goals.toList), proveAll(goals.toList, Bs.noBindings))
  }

  def proveAll(goals: List[R], bindings: Bs): List[Bs] = {
    if (bindings.equals(Bs.fail)) List(Bs.fail)
    else if (goals.isEmpty) List(bindings)
    else prove(goals.head, bindings).flatMap((bs: Bs) => proveAll(goals.tail, bs))
  }

  def prove(goal: R, bindings: Bs): List[Bs] = {
    getClauses(predicate(goal)).get.flatMap((c: C) => {
      val newClause = renameVariables(c)
      proveAll(List(newClause.body.get), unify(P(goal.value), P(newClause.head.value), bindings))
    })
  }

  def renameVariables(clause: C): C = {
    val headBindings = Bs(variablesIn(List(clause.head)).map((v: String) => v -> gensym(v)).toMap)
    val head = clause.head.value.map((s: String) => substitute(headBindings, s))
    if (clause.body.isDefined) {
      val bodyBindings = Bs(variablesIn(List(clause.body.get)).map((v: String) => v -> gensym(v)).toMap)
      val body = clause.body.get.value.map((s: String) => substitute(bodyBindings, s))
      C(R(head), Some(R(body)))
    } else {
      C(R(head), None)
    }
  }

  def variablesIn(goals: List[R]): List[String] = {
    goals.flatMap((g: R) => uniqueFindAnywhereif(isVariable, g))
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
    else vars.foreach((v: String) => println(s"$v = ${substBindings(bindings, P(v)).get}"))
  }
}
