package paip.chapter04

import paip.chapter04.Gps.Op

object BlocksWorld {
  def moveOns(a: String, b: String, c: String): Set[String] = {
    if (b.equals("table")) Set(s"$a on $c")
    else Set(s"$a on $c", s"space on $b")
  }

  def moveOp(a: String, b: String, c: String): Op = {
    Op(action = s"move $a from $b to $c",
      preconds = Set(s"space on $a", s"space on $c", s"$a on $b"),
      addList = moveOns(a, b, c),
      delList = moveOns(a, c, b))
  }

  def makeBlockOps(blocks: List[String]): List[Op] = {
    var ops: List[Op] = Nil
    for (a <- blocks)
      for (b <- blocks)
        if (!a.equals(b)) {
          for (c <- blocks)
            if (!c.equals(a) && !c.equals(b))
              ops = moveOp(a, b, c) :: ops
          ops = moveOp(a, "table", b) :: ops
          ops = moveOp(a, b, "table") :: ops
        }
    ops
  }
}
