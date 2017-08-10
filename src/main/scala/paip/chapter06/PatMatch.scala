package paip.chapter06

object PatMatch {

  abstract class P {
  }

  abstract class AtomP extends P {
    def value: String

    def isSegmentPattern: Boolean = value.contains("#")

    def isSinglePattern: Boolean = value.contains(":")

    def isVariable: Boolean = value.startsWith("?")

    def key: String = ???

    def variable: String = ???

    def equals(input: I): Boolean = value.equals(input.toString())
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
        else if (s.contains("?or") || s.contains("?and") || s.contains("?not")) SingleLogicalP(s)
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

  case class SingleLogicalP(value: String) extends SingleP

  case class SingleIsP(value: String) extends SingleP with Is {
    override def variable: String = value.split(":")(1)

    override def predicate: String = value.split(":")(2)
  }

  object SingleIsP {
    val predicateFnMap = Map[String, (Seq[String]) => Boolean](
      "isInt" -> isInt _
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

  def matchIs(pattern: SingleIsP, input: I, bindings: Bs): Bs = {
    val variable = pattern.variable
    val predicate = pattern.asInstanceOf[SingleIsP].predicate
    val newBindings = patMatch(VarP(variable), input, bindings)
    if (newBindings.equals(Bs.fail) || !SingleIsP.predicateFnMap(predicate).apply(input.value)) Bs.fail
    else newBindings
  }

  def matchOr(pattern: SingleLogicalP, input: I, bindings: Bs): Bs = {
    Bs()
  }

  def matchAnd(pattern: SingleLogicalP, input: I, bindings: Bs): Bs = {
    Bs()
  }

  def matchNot(pattern: SingleLogicalP, input: I, bindings: Bs): Bs = {
    Bs()
  }

  def segmentZeroOrMoreMatch(pattern: AtomP, input: I, bindings: Bs): Bs = {
    Bs()
  }

  def segmentOneOrMoreMatch(pattern: AtomP, input: I, bindings: Bs): Bs = {
    Bs()
  }

  def segmentZeroOrOneMatch(pattern: AtomP, input: I, bindings: Bs): Bs = {
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

  def main(args: Array[String]): Unit = {
    val result = patMatch(ConsP("x = ?is:?n:isInt"), I("x = 34"))
    result
  }
}
