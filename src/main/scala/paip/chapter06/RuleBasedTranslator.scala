package paip.chapter06

import paip.chapter05.Eliza.{Lhs, Rhs, Rule, switchViewpoints}
import paip.chapter05.PatMatchFacility._

object RuleBasedTranslator {
  implicit val elizaRules: List[Rule] = List(
    Rule(Lhs("?*X hello ?*Y"), Rhs("How do you do. Please state your problem")),
    Rule(Lhs("?*X I want ?*Y"), Rhs("What would it mean if you got ?Y", "Why do you want ?Y", "Suppose you got ?Y soon")),
    Rule(Lhs("?*X if ?*Y"), Rhs("Do you really think its likely that ?Y", "Do you wish that ?Y")),
    Rule(Lhs("?*X if ?*Y"), Rhs("Do you really think its likely that ?Y", "Do you wish that ?Y",
      "What do you think about ?y", "Really-- if ?Y")),
    Rule(Lhs("?*X no ?*Y"), Rhs("Why not?", "You are being a bit negative", "Are you saying \"NO\" just to be negative?")),
    Rule(Lhs("?*X I was ?*Y"), Rhs("Were you really?", "Perhaps I already knew you were ?Y",
      "Why do you tell me you were ?Y now?")),
    Rule(Lhs("?*X I feel ?*Y"), Rhs("Do you often feel ?Y ?")),
    Rule(Lhs("?*X I feet ?*Y"), Rhs("What other feelings do you have?"))
  )

  implicit val substitutions: Bs = Bs(B("I", "you"), B("you", "I"), B("me", "you"), B("am", "are"))

  def ruleIf(rule: Rule): P = {
    rule.lhs
  }

  def ruleBasedTranslator(input: String,
                          matcher: (P, I, Bs) => Bs = patMatch _,
                          ruleLhs: (Rule) => P = ruleIf _)
                         (implicit rules: List[Rule]) = {
    var result: Option[String] = None
    rules.find((rule: Rule) => {
      val bs = matcher.apply(ruleLhs.apply(rule), I(input), Bs.noBindings)
      if (!bs.equals(Bs.fail))
        result = Some(substitute(switchViewpoints(bs), rule.randomRhs))
      result.isDefined
    })
    result
  }

  def useElizaRules(input: String): Option[String] = {
    ruleBasedTranslator(input)
  }
}
