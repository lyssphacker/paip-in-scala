package paip.chapter04

import paip.chapter04.Gps.Op

object MonkeyAndBanans {
  implicit val bananaOps = List(
    Op(action = "climb-on-chair",
      preconds = Set("chair-at-middle-room", "at-middle-room", "on-floor"),
      addList = Set("at-bananas", "on-chair"),
      delList = Set("at-middle-room", "on-floor")),
    Op(action = "push-chair-from-door-to-middle-room",
      preconds = Set("chair-at-door", "at-door"),
      addList = Set("chair-at-middle-room", "at-middle-room"),
      delList = Set("chair-at-door", "at-door")),
    Op(action = "walk-from-door-to-middle-room",
      preconds = Set("at-door", "on-floor"),
      addList = Set("at-middle-room"),
      delList = Set("at-door")),
    Op(action = "grasp-bananas",
      preconds = Set("at-bananas", "empty-handed"),
      addList = Set("has-bananas"),
      delList = Set("empty-handed")),
    Op(action = "drop-ball",
      preconds = Set("has-ball"),
      addList = Set("empty-handed"),
      delList = Set("has-ball")),
    Op(action = "eat-bananas",
      preconds = Set("has-bananas"),
      addList = Set("empty-handed", "not-hungry"),
      delList = Set("has-bananas", "hungry"))
  )
}
