package paip.chapter11

import paip.chapter05.PatMatchFacility._

object Unify {
  def unify(x: P, y: P, bindings: Bs = Bs.noBindings): Bs = {
    if (bindings.equals(Bs.fail)) Bs.fail
    else if (x.equals(y)) bindings
    else if (x.isVariable)
      unifyVariable(x.toString, y, bindings)
    else if (y.isVariable)
      unifyVariable(y.toString, x, bindings)
    else if (x.isCons && y.isCons)
      unify(x.rest, y.rest,
        unify(x.first, y.first, bindings))
    else Bs.fail
  }

  def unifyVariable(variable: String, x: P, bindings: Bs): Bs = {
    if (bindings.getBinding(variable).isDefined)
      unify(P(bindings.lookup(variable)), x, bindings)
    else if (x.isVariable && bindings.getBinding(x.toString).isDefined)
      unify(P(variable), P(bindings.lookup(x.toString)), bindings)
    else bindings.extendBindings(variable, x.toString)
  }

  def main(args: Array[String]): Unit = {
//    val result = unify(P("?x + 1"), P("2 + ?y"))
//    val result = unify(P("?x"), P("?y"))
//    val result = unify(P("?x ?x"), P("?y ?y"))
//    val result = unify(P("?x ?x ?x"), P("?y ?y ?y"))
//    val result = unify(P("?x ?y"), P("?y ?x"))
    val result = unify(P("?x ?y a"), P("?y ?x ?x"))
    result
  }
}
