package paip.chapter06

object PatMatch {

  abstract class P {
  }

  abstract class AtomP extends P {
    def value: String

    def key: String = ???

    def variable: String = ???
  }

  case class ConsP(ps: List[AtomP]) extends P {
    def first: AtomP = ps.head

    def rest: ConsP = ConsP(ps.tail)

    def isEmpty: Boolean = ps.isEmpty

    def length: Int = ps.length
  }

  object ConsP {
    def apply(s: String) = {
      new ConsP(toPs(s.split(" ").toList))
    }
  }

  case class VarP(value: String) extends AtomP

  case class ConstP(value: String) extends AtomP

  abstract class SingleP extends AtomP {
    override def key: String = value.split(":")(0)
  }

  case class SingleLogicalP(value: String) extends SingleP {
    def patterns: List[P] = toPs(value.substring(value.indexOf(":")+1).split("\\.").toList)
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
    val singleLogicalMatchMap = Map(
      "?or" -> matchOr _,
      "?and" -> matchAnd _,
      "?not" -> matchNot _
    )

    val singleIsMatchMap = Map(
      "?is" -> matchIs _
    )
  }

  abstract class SegmentP extends AtomP {
    override def key: String = value.split("#")(0)

    override def variable: String = value.split("#")(1)
  }

  case class SegmentZeroOrMoreP(value: String) extends SegmentP {
    private val re = "(?<=\\()[^)]+(?=\\))".r

    def secondIndexOf(char: String, str: String): Int = {
      val lst = str.split(char).toList
      lst(0).length + lst(1).length + 2
    }

    private def getLst: List[String] = {
      if (value.contains("("))
        (for(m <- re.findAllIn(value)) yield m).toList
      else
        value.substring(secondIndexOf("#", value)).split("\\.").toList
    }

    def rest: ConsP = ConsP(PatMatch.toPs(getLst))

    def patterns: List[P] = toPs(value.substring(value.indexOf(":")+1).split("\\.").toList)
  }

  case class SegmentOneOrMoreP(value: String) extends SegmentP

  case class SegmentZeroOrOneP(value: String) extends SegmentP


  object SegmentP {
    val segmentZeroOrMoreMatchMap = Map[String, (SegmentZeroOrMoreP, I, Bs, Int) => Bs](
      "?*" -> segmentZeroOrMoreMatch _
    )

    val segmentMatchMap = Map[String, (SegmentP, I, Bs) => Bs](
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
        case p: SegmentZeroOrMoreP => segmentZeroOrMoreMatcher(p, input, bindings)
        case p: SegmentP => segmentMatcher(p, input, bindings)
        case p: SingleIsP => singleIsMatcher(p, input, bindings)
        case p: SingleLogicalP => singleLogicalMatcher(p, input, bindings)
        case p: VarP => matchVariable(p.value, input.toString, bindings)
        case cp: ConsP =>
          if (input.isEmpty && cp.isEmpty) bindings
          else
            cp.first match {
              case p: SegmentZeroOrMoreP if cp.length == 1 => patMatch(p, input, bindings)
              case _ => patMatch(cp.rest, input.rest, patMatch(cp.first, input.first, bindings))
            }
        case p: ConstP =>
          if (p.value.equals(input.toString)) bindings
          else Bs.fail
      }
    }

  }

  def segmentMatcher(p: SegmentP, input: I, bindings: Bs): Bs = {
    SegmentP.segmentMatchMap(p.key).apply(p, input, bindings)
  }

  def segmentZeroOrMoreMatcher(p: SegmentZeroOrMoreP, input: I, bindings: Bs): Bs = {
    SegmentP.segmentZeroOrMoreMatchMap(p.key).apply(p, input, bindings, 0)
  }

  def singleIsMatcher(p: SingleIsP, input: I, bindings: Bs): Bs = {
    SingleP.singleIsMatchMap(p.key).apply(p, input, bindings)
  }

  def singleLogicalMatcher(p: SingleLogicalP, input: I, bindings: Bs): Bs = {
    SingleP.singleLogicalMatchMap(p.key).apply(p, input, bindings)
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
    def matchOrPatterns(patterns: List[P], input: I, bindings: Bs): Bs = {
      if (patterns.isEmpty) Bs.fail
      else {
        val newBindings = patMatch(patterns.head, input, bindings)
        if (newBindings.equals(Bs.fail)) matchOrPatterns(patterns.tail, input, bindings)
        else newBindings
      }
    }
    matchOrPatterns(p.patterns, input, bindings)
  }

  def matchAnd(p: SingleLogicalP, input: I, bindings: Bs): Bs = {
    def matchAndPatterns(patterns: List[P], input: I, bindings: Bs): Bs = {
      if (bindings.equals(Bs.fail)) Bs.fail
      else if (patterns.isEmpty) bindings
      else {
        matchAndPatterns(patterns.tail, input, patMatch(patterns.head, input, bindings))
      }
    }
    matchAndPatterns(p.patterns, input, bindings)
  }

  def matchNot(p: SingleLogicalP, input: I, bindings: Bs): Bs = {
    val result = matchOr(p, input, bindings)
    if (result.equals(Bs.fail)) bindings
    else Bs.fail
  }

  def segmentZeroOrMoreMatch(p: SegmentZeroOrMoreP, input: I, bindings: Bs, start: Int = 0): Bs = {
    val variable = p.variable
    val pat = p.rest
    if (pat.isEmpty)
      matchVariable(variable, input.toString, bindings)
    else {
      val pos = firstMatchPos(pat.first, input, start)
      if (pos.isEmpty) Bs.fail
      else {
        val b2 = patMatch(
          pat,
          I(input.value.drop(pos.get).mkString(" ")),
          matchVariable(variable, input.value.slice(0, pos.get).mkString(" "), bindings)
        )
        if (b2.equals(Bs.fail)) segmentZeroOrMoreMatch(p, input, bindings, pos.get + 1)
        else b2
      }
    }
  }

  def firstMatchPos(pat1: P, input: I, start: Int): Option[Int] = {
    pat1 match {
      case p: ConstP => Some(input.value.indexOf(p.value, start))
      case _ =>
        if (start < input.value.length) Some(start)
        else None
    }
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

  def toPs(lst: List[String]): List[AtomP] = {
    lst.map((s: String) => {
      if (s.startsWith("?is")) SingleIsP(s)
      else if (s.startsWith("?or") || s.startsWith("?and") || s.startsWith("?not")) SingleLogicalP(s)
      else if (s.startsWith("?*")) SegmentZeroOrMoreP(s)
      else if (s.startsWith("?+")) SegmentOneOrMoreP(s)
      else if (s.startsWith("??")) SegmentZeroOrOneP(s)
      else if (s.startsWith("?")) VarP(s)
      else ConstP(s)
    })
  }

  def main(args: Array[String]): Unit = {
//    val result = patMatch(ConsP("?x ?or:<.=.> ?y"), I("3 < 4"))
//    val result = patMatch(ConsP("x = ?and:?is:?n:isInt.?is:?n:isOdd"), I("x = 3"))
//    val result = patMatch(ConsP("?x /= ?not:?x"), I("3 /= 4"))
//    val result = patMatch(ConsP("a ?*#?x#d"), I("a b c d"))
//    val result = patMatch(ConsP("a ?*#?x#(?*#?y#d)"), I("a b c d"))
    val result = patMatch(ConsP("a ?*#?x#(?*#?y#?x.?y)"), I("a b c d b c d"))
    result
  }
}
