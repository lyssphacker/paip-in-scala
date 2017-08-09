package paip.chapter06

object PatMatch {
  abstract class P {
    def value: List[String]

    def rest: P

    def first: P

    def key: String = ???

    def variable: String = value.head

    def isSegmentPattern: Boolean = value.head.contains("#")

    def isSinglePattern: Boolean = value.head.contains(":")

    def firstValue: String = value.head

    def isVariable: Boolean = value.length == 1 && value.head.startsWith("?")

    override def toString: String = value mkString " "

    def equals(input: I): Boolean = toString().equals(input.toString())

    def isCons: Boolean = value.length > 1

    def isEmpty: Boolean = value.isEmpty
  }

  case class ConsP(value: List[String]) extends P {
    override def first: ConsP = ConsP(value.head)

    override def rest: ConsP = ConsP(value.tail)
  }

  object ConsP {
    def apply(value: String) = new ConsP(value.split(" ").toList)
  }

  case class VarConstP(value: List[String]) extends P {
    override def first: VarConstP = VarConstP(value)

    override def rest: VarConstP = VarConstP(value)
  }

  object VarConstP {
    def apply(value: String) = new VarConstP(List(value))
  }

  abstract class SingleP extends P {
    override def key: String = value.head.split(":")(0)
  }

  case class SingleLogicalP(value: List[String]) extends SingleP {
    override def first: SingleLogicalP = SingleLogicalP(value.head)

    override def rest: SingleLogicalP = SingleLogicalP(value.tail)
  }

  object SingleLogicalP {
    def apply(value: String) = new SingleLogicalP(value.split(" ").toList)
  }

  case class SingleIsP(value: List[String]) extends SingleP with Is {
    override def first: SingleIsP = SingleIsP(value.head)

    override def rest: SingleIsP = SingleIsP(value.tail)

    override def variable: String = value.head.split(":")(1)

    override def predicate: String = value.head.split(":")(2)
  }

  object SingleIsP {
    val predicateFnMap = Map[String, (Seq[String]) => Boolean](
      "isNumber" -> isNumber _
    )
    def apply(value: String) = new SingleIsP(value.split(" ").toList)
  }

  trait Is {
    def predicate: String
  }

  object SingleP {
    val singleMatch = Map(
      "?is" -> matchIs _,
      "?or" -> matchOr _,
      "?and" -> matchAnd _,
      "?not" -> matchNot _
    )
  }

  case class SegmentP(value: List[String]) extends P {
    override def rest: SegmentP = SegmentP(value.tail)

    override def first: SegmentP = SegmentP(value.head)

    override def key: String = value.head.split("#")(0)

    override def variable: String = value.head.split("#")(1)
  }

  object SegmentP {
    val segmentMatch = Map(
      "?*" -> segmentZeroOrMoreMatch _,
      "?+" -> segmentOneOrMoreMatch _,
      "??" -> segmentZeroOrOneMatch _
    )

    def apply(value: String) = new SegmentP(value.split(" ").toList)
  }

  case class I(value: List[String]) {
    override def toString: String = value mkString " "

    def isCons: Boolean = value.length > 1

    def first: I = I(value.head)

    def rest: I = I(value.tail)
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
    else if (pattern.isSegmentPattern)
      segmentMatcher(pattern, input, bindings)
    else if (pattern.isSinglePattern)
      singleMatcher(pattern, input, bindings)
    else if (pattern.isVariable)
      matchVariable(pattern.toString, input.toString, bindings)
    else if (pattern.equals(input)) bindings
    else if (pattern.isCons && input.isCons)
      patMatch(pattern.rest, input.rest,
        patMatch(pattern.first, input.first, bindings))
    else Bs.fail
  }

  def segmentMatcher(pattern: P, input: I, bindings: Bs): Bs = {
    getSegmentMatchFn(pattern).apply(pattern, input, bindings)
  }

  def getSegmentMatchFn(p: P): ((P, I, Bs) => Bs) = {
    SegmentP.segmentMatch(p.key)
  }

  def singleMatcher(pattern: P, input: I, bindings: Bs): Bs = {
    getSingleMatchFn(pattern).apply(pattern, input, bindings)
  }

  def getSingleMatchFn(p: P): ((P, I, Bs) => Bs) = {
    SingleP.singleMatch(p.key)
  }

  def matchVariable(variable: String, input: String, bindings: Bs): Bs = {
    val binding: Option[B] = bindings.getBinding(variable)
    if (binding.isEmpty) bindings.extendBindings(variable.replace("*", ""), input)
    else if (input.equals(binding.get.bindingVal)) bindings
    else Bs.fail
  }

  def matchIs(pattern: P, input: I, bindings: Bs): Bs = {
    val variable = pattern.variable
    val predicate = pattern.asInstanceOf[SingleIsP].predicate
    val newBindings = patMatch(VarConstP(variable), input, bindings)
    if (newBindings.equals(Bs.fail) || !SingleIsP.predicateFnMap(predicate).apply(input.value)) Bs.fail
    else newBindings
  }

  def matchOr(pattern: P, input: I, bindings: Bs): Bs = {
    Bs()
  }

  def matchAnd(pattern: P, input: I, bindings: Bs): Bs = {
    Bs()
  }

  def matchNot(pattern: P, input: I, bindings: Bs): Bs = {
    Bs()
  }

  def segmentZeroOrMoreMatch(pattern: P, input: I, bindings: Bs): Bs = {
    Bs()
  }

  def segmentOneOrMoreMatch(pattern: P, input: I, bindings: Bs): Bs = {
    Bs()
  }

  def segmentZeroOrOneMatch(pattern: P, input: I, bindings: Bs): Bs = {
    Bs()
  }

  def isNumber(s: String*): Boolean = {
    true
  }

  def main(args: Array[String]): Unit = {
    val result = patMatch(ConsP("x = ?is:?n:isNumber"), I("x = 34"))
    result
  }
}
