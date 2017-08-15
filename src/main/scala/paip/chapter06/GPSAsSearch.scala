package paip.chapter06

import paip.chapter04.BlocksWorld.makeBlockOps
import paip.chapter04.FullGps.{convertOp, countIf, isAction}
import paip.chapter04.Gps.Op
import paip.chapter06.Search.{beamSearch, findAllIf}
import paip.DebugUtils._

object GPSAsSearch {
  implicit val convertedBlocksWorldOps = makeBlockOps(List("a", "b", "c")).map(convertOp)

  def isSubset[T](lst1: List[T], lst2: List[T]): Boolean = {
    lst1.forall(lst2.contains)
  }

  def applicableOps(state: List[String])(implicit ops: List[Op]): List[Op] = {
    findAllIf[Op]((op: Op) => isSubset(op.preconds.toList, state), ops)
  }

  def gpsSuccessors(state: List[String]): List[List[String]] = {
    applicableOps(state).map((op: Op) => {
      val first = state.filter((s: String) => !op.delList.contains(s))
      val second = op.addList.toList
      first ::: second
    })
  }

  def searchGps(start: List[String], goal: List[String], beamWidth: Int = 10): List[String] = {
    findAllIf[String](
      isAction,
      beamSearch[List[String], Int](
        "start" :: start,
        (state: List[String]) => goal.forall(state.contains),
        gpsSuccessors,
        (state: List[String]) => countIf(isAction, state) +
          countIf((con: String) => !state.contains(con), goal),
        beamWidth
      ).get
    )
  }

  def main(args: Array[String]): Unit = {
    debug("search")
    val result = searchGps(List("c on a", "a on table", "b on table", "space on c",
      "space on b", "space on table"), List("b on c", "a on b"))
    result
  }
}
