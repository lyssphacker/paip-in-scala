package paip.chapter06

object PatMatch {

  abstract class P {
  }

  abstract class AtomP extends P {
    def value: String

    def key: String = ???

    def variable: String = ???
  }

  case class ConsP(ps: List[P]) extends P {
    def first: P = ps.head

    def rest: ConsP = ConsP(ps.tail)

    def isEmpty: Boolean = ps.isEmpty
  }

  object ConsP {
    def apply(s: String) = {
      val ps = s.split(" ").toList.map((s: String) => {
        if (s.startsWith("?is")) SingleIsP(s)
        else if (s.startsWith("?or") || s.startsWith("?and") || s.startsWith("?not")) SingleLogicalP(s)
        else if (s.contains("#")) SegmentP(s)
        else if (s.startsWith("?")) VarP(s)
        else ConstP(s)
      })
      new ConsP(ps)
    }
  }

  case class VarP(value: String) extends AtomP

  case class ConstP(value: String) extends AtomP

  abstract class SingleP extends AtomP {
    override def key: String = value.split(":")(0)
  }

  case class SingleLogicalP(value: String) extends SingleP {
    def patterns: List[String] = value.substring(value.indexOf(":")+1).split("\\.").toList
  }

  case class SingleIsP(value: String) extends SingleP with Is {
    override def variable: String = value.split(":")(1)

    override def predicate: String = value.split(":")(2)
  }

  object SingleIsP {
    val predicateFnMap = Map[String, (Seq[String]) => Boolean](
      "isInt" -> isInt _,
      "isOdd" -> isOdd _
    )
  }

  trait Is {
    def predicate: String
  }

  object SingleP {
    val singleLogicalMatch = Map(
      "?or" -> matchOr _,
      "?and" -> matchAnd _,
      "?not" -> matchNot _
    )

    val singleIsMatch = Map(
      "?is" -> matchIs _
    )
  }

  case class SegmentP(value: String) extends AtomP {
    override def key: String = value.split("#")(0)

    override def variable: String = value.split("#")(1)
  }

  object SegmentP {
    val segmentMatch = Map(
      "?*" -> segmentZeroOrMoreMatch _,
      "?+" -> segmentOneOrMoreMatch _,
      "??" -> segmentZeroOrOneMatch _
    )
  }

  case class I(value: List[String]) {
    override def toString: String = value mkString " "

    def first: I = I(value.head)

    def rest: I = I(value.tail)

    def isEmpty: Boolean = value.isEmpty
  }

  object I {
    def apply(value: String) = new I(value.split(" ").toList)
  }

  case class B(variable: String, value: String) {
    def bindingVar: String = variable

    def bindingVal: String = value
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
    else {
      pattern match {
        case cp: ConsP =>
          if (input.isEmpty && cp.isEmpty) bindings
          else patMatch(cp.rest, input.rest, patMatch(cp.first, input.first, bindings))
        case p: SegmentP => segmentMatcher(p, input, bindings)
        case p: SingleIsP => singleIsMatcher(p, input, bindings)
        case p: SingleLogicalP => singleLogicalMatcher(p, input, bindings)
        case p: VarP => matchVariable(p.value, input.toString, bindings)
        case p: ConstP =>
          if (p.value.equals(input.toString)) bindings
          else Bs.fail
      }
    }

  }

  def segmentMatcher(pattern: AtomP, input: I, bindings: Bs): Bs = {
    getSegmentMatchFn(pattern).apply(pattern, input, bindings)
  }

  def getSegmentMatchFn(p: AtomP): ((AtomP, I, Bs) => Bs) = {
    SegmentP.segmentMatch(p.key)
  }

  def singleIsMatcher(pattern: SingleIsP, input: I, bindings: Bs): Bs = {
    getSingleIsMatchFn(pattern).apply(pattern, input, bindings)
  }

  def singleLogicalMatcher(pattern: SingleLogicalP, input: I, bindings: Bs): Bs = {
    getSingleLogicalMatchFn(pattern).apply(pattern, input, bindings)
  }

  def getSingleIsMatchFn(p: SingleIsP): ((SingleIsP, I, Bs) => Bs) = {
    SingleP.singleIsMatch(p.key)
  }

  def getSingleLogicalMatchFn(p: SingleLogicalP): ((SingleLogicalP, I, Bs) => Bs) = {
    SingleP.singleLogicalMatch(p.key)
  }

  def matchVariable(variable: String, input: String, bindings: Bs): Bs = {
    val binding: Option[B] = bindings.getBinding(variable)
    if (binding.isEmpty) bindings.extendBindings(variable.replace("*", ""), input)
    else if (input.equals(binding.get.bindingVal)) bindings
    else Bs.fail
  }

  def matchIs(p: SingleIsP, input: I, bindings: Bs): Bs = {
    val variable = p.variable
    val predicate = p.predicate
    val newBindings = patMatch(VarP(variable), input, bindings)
    if (newBindings.equals(Bs.fail) || !SingleIsP.predicateFnMap(predicate).apply(input.value)) Bs.fail
    else newBindings
  }

  def matchOr(p: SingleLogicalP, input: I, bindings: Bs): Bs = {
    def matchOrPatterns(patterns: List[String], input: I, bindings: Bs): Bs = {
      if (patterns.isEmpty) Bs.fail
      else {
        val newBindings = patMatch(ConstP(patterns.head), input, bindings)
        if (newBindings.equals(Bs.fail)) matchOrPatterns(patterns.tail, input, bindings)
        else newBindings
      }
    }
    matchOrPatterns(p.patterns, input, bindings)
  }

  def matchAnd(p: SingleLogicalP, input: I, bindings: Bs): Bs = {
    def matchAndPatterns(patterns: List[String], input: I, bindings: Bs): Bs = {
      if (bindings.equals(Bs.fail)) Bs.fail
      else if (patterns.isEmpty) bindings
      else {
        val pattern = patterns.head
        if (pattern.startsWith("?is"))
          matchAndPatterns(patterns.tail, input, patMatch(SingleIsP(pattern), input, bindings))
        else
          matchAndPatterns(patterns.tail, input, patMatch(SingleLogicalP(pattern), input, bindings))
      }
    }
    matchAndPatterns(p.patterns, input, bindings)
  }

  def matchNot(p: SingleLogicalP, input: I, bindings: Bs): Bs = {
    val result = matchOr(p, input, bindings)
    if (result.equals(Bs.fail)) bindings
    else Bs.fail
  }

  def segmentZeroOrMoreMatch(p: AtomP, input: I, bindings: Bs): Bs = {
    Bs()
  }

  def segmentOneOrMoreMatch(p: AtomP, input: I, bindings: Bs): Bs = {
    Bs()
  }

  def segmentZeroOrOneMatch(p: AtomP, input: I, bindings: Bs): Bs = {
    Bs()
  }

  def isInt(s: String*): Boolean = {
    if (s.length != 1) false
    else {
      var result = true
      val value = s(0)
      try {
        value.toInt
      } catch {
        case e: NumberFormatException => result = false
      }
      result
    }
  }

  def isOdd(s: String*): Boolean = {
    if (s.length != 1) false
    else {
      var result = true
      val value = s(0)
      try {
        val asInt = value.toInt
        result = asInt %2 != 0
      } catch {
        case e: NumberFormatException => result = false
      }
      result
    }
  }

  def main(args: Array[String]): Unit = {
//    val result = patMatch(ConsP("x = ?and:?is:?n:isInt.?is:?n:isOdd"), I("x = 3"))
    val result = patMatch(ConsP("?x /= ?not:?x"), I("x = 3"))
    result
  }
}
