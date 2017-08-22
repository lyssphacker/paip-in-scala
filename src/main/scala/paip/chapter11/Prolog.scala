package paip.chapter11

import paip.chapter05.PatMatchFacility.{Bs, P}
import paip.chapter11.Prolog1.{addClauses, proveGoals, _}
import paip.chapter11.Unify._

object Prolog {

  var showPrologVarsMap: scala.collection.mutable.Map[String, (List[String], Bs, List[R]) => Bs] = scala.collection.mutable.Map.empty

  def replaceAnonVars(goals: List[R]): List[R] = {
    goals.map((g: R) => replaceAnonVar(g))
  }

  def replaceAnonVar(goal: R): R = {
    R(goal.value.mkString(" ").replace("?", gensym("?")))
  }

  def replaceAnonVars(clause: C): C = {
    C(replaceAnonVar(clause.head), replaceAnonVars(clause.body))
  }

  def addClauses(clauses: C*): Unit = {
    for (clause <- clauses) addClause(replaceAnonVars(clause))
  }

  def prove(goal: R, bindings: Bs, otherGoals: List[R]): Bs = {
    val clauses: Option[List[C]] = getClauses(predicate(goal))
    if (clauses.isDefined) {
      var result = Bs.fail
      clauses.get.find((c: C) => {
        val newClause = renameVariables(c)
        result = proveAll(otherGoals ::: newClause.body,
          unify(P(goal.value), P(newClause.head.value), bindings))
        !result.equals(Bs.fail)
      })
      result
    } else {
      showPrologVarsMap(predicate(goal)).apply(goal.rest, bindings, otherGoals)
    }
  }

  def proveAll(goals: List[R], bindings: Bs): Bs = {
    if (bindings.equals(Bs.fail)) Bs.fail
    else if (goals.isEmpty) bindings
    else prove(goals.head, bindings, goals.tail)
  }

  def proveGoals(goals: R*): Unit = {
    proveAll(goals.toList :+ createshowPrologVarsR(goals.toList), Bs.noBindings)
    println("No.")
  }

  def createshowPrologVarsR(goals: List[R]): R = {
    val vars = variablesIn(goals, isNonAnonVariable)
    R("showPrologVars " + vars.mkString(" "))
  }

  def isNonAnonVariable(variable: String): Boolean = {
    isVariable(variable) && !variable.equals("?")
  }

  def showPrologVars(vars: List[String], bindings: Bs, otherGoals: List[R]): Bs = {
    if (vars.isEmpty) println("Yes.")
    else {
      vars.foreach((v: String) => println(s"$v = ${substBindings(bindings, P(v))}"))
    }

    if (continue()) Bs.fail
    else proveAll(otherGoals, bindings)
  }

  def continue(): Boolean = {
    val input = scala.io.StdIn.readChar()
    input match {
      case ';' => true
      case '.' => false
      case '-' => continue()
      case _ => {
        println("Type ; to see more or . to stop")
        continue()
      }
    }
  }

  def main(args: Array[String]): Unit = {
    showPrologVarsMap.put("showPrologVars", showPrologVars _)

    addClauses(C(R("likes Kim Robin")))
    addClauses(C(R("likes Sandy Lee")))
    addClauses(C(R("likes Sandy Kim")))
    addClauses(C(R("likes Robin cats")))
    addClauses(C(R("likes Sandy ?x"), R("likes ?x cats")))
    addClauses(C(R("likes Sandy ?x"), R("likes ?x cats"), R("likes ?x Kim")))
    addClauses(C(R("likes ?x ?x")))

        proveGoals(R("likes Sandy ?who"))
//    proveGoals(R("likes ?who Sandy"))
    //    proveGoals(R("likes Robin Lee"))
    //    proveGoals(R("likes ?x ?y"), R("likes ?y ?x"))
  }
}
