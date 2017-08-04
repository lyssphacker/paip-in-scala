package paip.chapter04

object Utils04 {
  implicit def intWithTimes(n: Int) = new {
    def times(f: => Unit) = 1 to n foreach { _ => f }
  }

  def dbgIndent(indent: Int, str: String): Unit = {
    indent times {
      print(" ")
    }
    println(str)
  }
}
