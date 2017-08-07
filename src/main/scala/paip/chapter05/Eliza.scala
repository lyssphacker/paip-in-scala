package paip.chapter05

import paip.chapter05.PatMatch._
import paip.chapter05.Utils05.randomElt

import scala.util.control.Breaks.{break, breakable}

object Eliza {
  def Lhs = P

  case class Rule(lhs: P, rhs: Rhs) {
    def randomRhs: String = randomElt(rhs.values).toString
  }

  case class Rhs(values: List[P])

  object Rhs {
    def apply(values: String*) = new Rhs(values.toList.map((s: String) => P(s)))
  }

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

  def replaceLiterary(str: String, map: Map[String, String]): String = {
    var result = str
    breakable(
      for ((k, v) <- map) {
        if (k.equals(str)) {
          result = str.replace(k, v)
          break
        }
      }
    )
    result
  }

  def switchViewpoints(bs: Bs)(implicit substitutions: Bs): Bs = {
    var result = Bs()
    for ((k, v) <- bs.bindings) {
      result = result.extendBindings(k, replaceLiterary(v, substitutions.bindings))
    }
    result
  }

  def useElizaRules(input: String)(implicit rules: List[Rule]): Option[String] = {
    var result: Option[String] = None
    rules.find((rule: Rule) => {
      val bs = patMatch(rule.lhs, I(input))
      if (!bs.equals(Bs.fail))
        result = Some(substitute(switchViewpoints(bs), rule.randomRhs))
      result.isDefined
    })
    result
  }

  def main(args: Array[String]): Unit = {
    while (true) {
      print("eliza>")
      val input = scala.io.StdIn.readLine()
      val output = useElizaRules(input)
      if (output.isDefined) println(output.get)
      else println("do not understand")
    }
  }
}
