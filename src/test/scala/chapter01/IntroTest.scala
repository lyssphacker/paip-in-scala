package test.chapter01

import org.scalatest.FunSuite
import chapter01.Intro

/**
  * Created by lixkid on 01.01.17..
  */
class IntroTest extends FunSuite {

  test("testLastName") {
    val lst = List("Hrvoje", "Krot")
    assert(Intro.lastName(lst) == "Krot")
  }

  test("testFirstName") {
    val lst1 = List("John", "Q", "Public")
    assert(Intro.firstName(lst1) == "John")

    val lst2 = List("Malcom", "X")
    assert(Intro.firstName(lst2) == "Malcom")

    val lst3 = List("Admiral", "Grace", "Murray", "Hopper")
    assert(Intro.firstName(lst3) == "Grace")

    val lst4 = List("Spot")
    assert(Intro.firstName(lst4) == "Spot")

    val lst5 = List("Aristotle")
    assert(Intro.firstName(lst5) == "Aristotle")

    val lst6 = List("A", "A", "Milne")
    assert(Intro.firstName(lst6) == "A")

    val lst7 = List("Z", "Z", "Top")
    assert(Intro.firstName(lst7) == "Z")

    val lst8 = List("Sir", "Larry", "Olivier")
    assert(Intro.firstName(lst8) == "Larry")

    val lst9 = List("Miss", "Scarlet")
    assert(Intro.firstName(lst9) == "Scarlet")

    val lst10 = List("Madam", "Major", "General", "Paul", "Jones")
    assert(Intro.firstName(lst10) == "Paul")
  }

  test("testMappend") {
    def selfAndDouble(x: Any): List[Int] = {
      val result = x match {
        case a: Int => List(a, a + a)
        case _ => List.empty
      }

      return result
    }

    val lst1 = List(1, 10, 1000)
    assert(Intro.mappend(selfAndDouble, lst1) == List(1, 2, 10, 20, 1000, 2000))
    val lst2 = List("testing", 1, 10, 1000)
    assert(Intro.mappend(selfAndDouble, lst2) == List(1, 2, 10, 20, 1000, 2000))

    def fourElementList(x: Any): List[Any] = {
      List(List(x, x), List(x, x))
    }

    val lst3 = List(1, 2)
    assert(Intro.mappend(fourElementList, lst3) == List(List(1, 1), List(1, 1), List(2, 2), List(2, 2)))
  }

  test("testNumbersAndNegations") {
    val lst1 = List(1, 10, 1000)
    assert(Intro.numbersAndNegations(lst1) == List(1, -1, 10, -10, 1000, -1000))
    val lst2 = List("testing", 1, 10, 1000)
    assert(Intro.numbersAndNegations(lst2) == List(1, -1, 10, -10, 1000, -1000))
  }
}
