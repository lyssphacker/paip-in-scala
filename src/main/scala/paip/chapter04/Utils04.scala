package paip.chapter04

object Utils04 {
  var dbgIds: Set[String] = Set.empty

  implicit def intWithTimes(n: Int) = new {
    def times(f: => Unit) = 1 to n foreach { _ => f }
  }

  def dbgIndent(id: String, indent: Int, str: String): Unit = {
    if (dbgIds.contains(id)) {
      indent times {
        print(" ")
      }
      println(str)
    }
  }

  def dbg(id: String, str: String): Unit = {
    if (dbgIds.contains(id)) {
      println(str)
    }
  }

  def debug(id: String): Unit = {
    dbgIds = dbgIds + id
  }

  def undebug(id: String): Unit = {
    dbgIds = dbgIds - id
  }
}
