package paip.chapter07

import paip.chapter05.Eliza._

object Student {
  implicit val studentRules: List[Rule] = List(
    Rule(Lhs("?x* ."), Rhs("?x")),
    Rule(Lhs("?x* . ?y*"), Rhs("?x ?y")),
    Rule(Lhs("if ?x* , then ?y*"), Rhs("?x ?y")),
    Rule(Lhs("if ?x* then ?y*"), Rhs("?x ?y")),
    Rule(Lhs("if ?x* , ?y*"), Rhs("?x ?y")),
    Rule(Lhs("?x* , and ?y*"), Rhs("?x ?y")),
    Rule(Lhs("find ?x* and ?y*"), Rhs("= to-find-1 ?x", "= to-find-2 ?y")),
    Rule(Lhs("find ?x*"), Rhs("= to-find ?x")),
    Rule(Lhs("?x* equals ?y*"), Rhs("= ?x ?y")),
    Rule(Lhs("?x* same as ?y*"), Rhs("= ?x ?y")),
    Rule(Lhs("?x* = ?y*"), Rhs("= ?x ?y")),
    Rule(Lhs("?x* is equal to ?y*"), Rhs("= ?x ?y")),
    Rule(Lhs("?x* is ?y*"), Rhs("= ?x ?y")),
    Rule(Lhs("?x* - ?y*"), Rhs("- ?x ?y")),
    Rule(Lhs("?x* minus ?y*"), Rhs("- ?x ?y")),
    Rule(Lhs("difference between ?x* and ?y*"), Rhs("- ?y ?x")),
  )
}
