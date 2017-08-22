package paip.chapter11

import paip.chapter11.Prolog1._

object Prolog {
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
}
