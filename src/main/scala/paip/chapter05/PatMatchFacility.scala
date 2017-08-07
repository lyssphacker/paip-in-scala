package paip.chapter05

import scala.language.postfixOps

object PatMatchFacility {

  case class P(value: List[String]) {
    def first: P = {
      P(value.head)
    }

    def firstValue: String = {
      value.head
    }

    def rest: P = {
      P(value.tail)
    }

    def isVariable: Boolean = {
      value.length == 1 && value.head.startsWith("?")
    }

    def isSegmentVariable: Boolean = {
      value.head.startsWith("?*")
    }

    override def toString: String = {
      value mkString " "
    }

    def equals(input: I): Boolean = {
      toString().equals(input.toString())
    }

    def isCons: Boolean = {
      value.length > 1
    }

    def isEmpty: Boolean = {
      value.isEmpty
    }
  }

  case class I(value: List[String]) {
    override def toString: String = {
      value mkString " "
    }

    def isCons: Boolean = {
      value.length > 1
    }

    def first: I = {
      I(value.head)
    }

    def rest: I = {
      I(value.tail)
    }
  }

  object P {
    def apply(value: String) = new P(value.split(" ").toList)
  }

  object I {
    def apply(value: String) = new I(value.split(" ").toList)
  }

  case class B(variable: String, value: String) {
    def bindingVar: String = {
      variable
    }

    def bindingVal: String = {
      value
    }
  }

  case class Bs(bindings: Map[String, String]) {
    def getBinding(variable: String): Option[B] = {
      val value: Option[String] = bindings.get(variable)
      if (value.isEmpty) None
      else Some(B(variable, value.get))
    }

    def lookup(variable: String): String = {
      bindings(variable)
    }

    def extendBindings(variable: String, value: String): Bs = {
      if (bindings.equals(Bs.noBindings.bindings)) Bs(Map(variable -> value))
      else Bs(bindings + (variable -> value))
    }
  }

  object Bs {
    val fail = Bs()
    val noBindings = Bs(B("t", "t"))

    def apply(bindings: B*) = new Bs(bindings map (b => b.variable -> b.value) toMap)
  }

  def patMatch(pattern: P, input: I, bindings: Bs = Bs.noBindings): Bs = {
    if (bindings.equals(Bs.fail)) Bs.fail
    else if (pattern.isSegmentVariable)
      segmentMatch(pattern, input, bindings)
    else if (pattern.isVariable)
      matchVariable(pattern.toString, input.toString, bindings)
    else if (pattern.equals(input)) bindings
    else if (pattern.isCons && input.isCons)
      patMatch(pattern.rest, input.rest,
        patMatch(pattern.first, input.first, bindings))
    else Bs.fail
  }

  def matchVariable(variable: String, input: String, bindings: Bs): Bs = {
    val binding: Option[B] = bindings.getBinding(variable)
    if (binding.isEmpty) bindings.extendBindings(variable.replace("*", ""), input)
    else if (input.equals(binding.get.bindingVal)) bindings
    else Bs.fail
  }

  def substitute(bs: Bs, value: String): String = {
    bs.bindings.foldLeft(value)((a, b) => a.replaceAllLiterally(b._1, b._2))
  }

  def segmentMatch(pattern: P, input: I, bindings: Bs, start: Int = 0): Bs = {
    val variable = pattern.firstValue
    val pat = pattern.rest
    if (pat.isEmpty)
      matchVariable(variable, input.toString, bindings)
    else {
      val pos = input.value.indexOf(pat.firstValue, start)
      if (pos == -1) Bs.fail
      else {
        val b2 = patMatch(
          pat,
          I(input.value.drop(pos).mkString(" ")),
          matchVariable(variable, input.value.slice(0, pos).mkString(" "), bindings)
        )
        if (b2.equals(Bs.fail)) segmentMatch(pattern, input, bindings, pos + 1)
        else b2
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val result = patMatch(P("?*X a b ?*X"), I("1 2 a b a b 1 2 a b"))
    result
  }
}
