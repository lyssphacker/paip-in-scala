package paip.chapter11

import paip.chapter05.PatMatchFacility._

object Unify {
  implicit val checkOccurs = false

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

  def unifyVariable(variable: String, x: P, bindings: Bs)(implicit checkOccurs: Boolean): Bs = {
    if (bindings.getBinding(variable).isDefined)
      unify(P(bindings.lookup(variable)), x, bindings)
    else if (x.isVariable && bindings.getBinding(x.toString).isDefined)
      unify(P(variable), P(bindings.lookup(x.toString)), bindings)
    else if (checkOccurs && occursCheck(variable, x, bindings)) Bs.fail
    else bindings.extendBindings(variable, x.toString)
  }

  def occursCheck(variable: String, x: P, bindings: Bs): Boolean = {
    if (variable.equals(x.toString)) true
    else if (x.isVariable && bindings.getBinding(x.toString).isDefined)
      occursCheck(variable, P(bindings.lookup(x.toString)), bindings)
    else if (x.isCons)
      occursCheck(variable, x.first, bindings) || occursCheck(variable, x.rest, bindings)
    else false
  }

  def substitute(bs: Bs, value: String): String = {
    bs.bindings.foldLeft(value)((a, b) => a.replaceAllLiterally(b._1, b._2))
  }

  def substBindings(bindings: Bs, x: P): Option[P] = {
    if (bindings.equals(Bs.fail)) None
    else if (bindings.equals(Bs.noBindings)) Some(x)
    else if (x.isVariable && bindings.getBinding(x.toString).isDefined)
      substBindings(bindings, P(bindings.lookup(x.toString)))
    else if (x.isAtom) Some(x)
    else reuseCons(substBindings(bindings, x.first), substBindings(bindings, x.rest), x)
  }

  def reuseCons(x: Option[P], y: Option[P], xy: P): Option[P] = {
    if (x.isEmpty || y.isEmpty) None
    else if (x.get.equals(xy.first) && y.get.equals(xy.rest)) Some(xy)
    else Some(P(x.get.value ::: y.get.value))
  }

  def unifier(x: P, y: P): Option[P] = {
    substBindings(unify(x, y), x)
  }

  def main(args: Array[String]): Unit = {
//    val result = unify(P("a a"), P("a a"))
//    val result = unifier(P("?x ?y a"), P("?y ?x ?x"))
//    val result = unifier(P("(?a * ?x ^ 2) + (?b * ?x) + ?c"), P("?z + (4 * 5) + 3"))
//    val result = unify(P("?x"), P("f ?x"))
    val result = unify(P("?x ?y"), P("f ?y f ?x"))
    result
  }
}
