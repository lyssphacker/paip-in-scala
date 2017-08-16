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
    Rule(Lhs("find ?x* and ?y*"), Rhs("to-find-1 = ?x", "to-find-2 = ?y")),
    Rule(Lhs("find ?x*"), Rhs("to-find = ?x")),
    Rule(Lhs("?x* equals ?y*"), Rhs("?x = ?y")),
    Rule(Lhs("?x* same as ?y*"), Rhs("?x = ?y")),
    Rule(Lhs("?x* = ?y*"), Rhs("?x = ?y")),
    Rule(Lhs("?x* is equal to ?y*"), Rhs("?x = ?y")),
    Rule(Lhs("?x* is ?y*"), Rhs("?x = ?y")),
    Rule(Lhs("?x* - ?y*"), Rhs("?x - ?y")),
    Rule(Lhs("?x* minus ?y*"), Rhs("?x - ?y")),
    Rule(Lhs("difference between ?x* and ?y*"), Rhs("?y - ?x")),
    Rule(Lhs("difference ?x* and ?y*"), Rhs("?y - ?x")),
    Rule(Lhs("?x* + ?y*"), Rhs("?x + ?y")),
    Rule(Lhs("?x* plus ?y*"), Rhs("?x + ?y")),
    Rule(Lhs("sum ?x* and ?y*"), Rhs("?x + ?y")),
    Rule(Lhs("product ?x* and ?y*"), Rhs("?x * ?y")),
    Rule(Lhs("?x* * ?y*"), Rhs("?x * ?y")),
    Rule(Lhs("?x* times ?y*"), Rhs("?x * ?y")),
    Rule(Lhs("?x* / ?y*"), Rhs("?x / ?y")),
    Rule(Lhs("?x* per ?y*"), Rhs("?x / ?y")),
    Rule(Lhs("?x* divided by ?y*"), Rhs("?x / ?y")),
    Rule(Lhs("half ?x*"), Rhs("?x / 2")),
    Rule(Lhs("one half ?x*"), Rhs("?x / 2")),
    Rule(Lhs("twice ?x*"), Rhs("2 * ?x")),
    Rule(Lhs("square ?x*"), Rhs("?x * ?x")),
    Rule(Lhs("?x* % less than ?y*"), Rhs("?y * ((100 - ?x) / 100))")),
    Rule(Lhs("?x* % more than ?y*"), Rhs("?y * ((100 + ?x) / 100))")),
    Rule(Lhs("?x* % ?y*"), Rhs("(?x / 100) * ?y"))
  )
}
