package paip.chapter05

import paip.chapter05.PatMatch.P

object Eliza {
  def Lhs = P

  case class Rule(lhs: P, rhs: Rhs)

  case class Rhs(values: List[P])

  object Rhs {
    def apply(values: String*) = new Rhs(values.toList.map((s: String) => P(s)))
  }

  implicit val elizaRules: List[Rule] = List(
    Rule(Lhs("?*X hello ?*Y"), Rhs("How do you do. Please state your problem")),
    Rule(Lhs("?*X I want ?*Y"), Rhs("What would it mean if you got ?Y", "Why do you want ?Y", "Suppose you got ?Y soon"))
  )
}
