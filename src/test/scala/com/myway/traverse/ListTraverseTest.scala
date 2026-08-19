package com.myway.traverse

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ListTraverseTest extends AnyFunSuite with Matchers {

  test("traverse") {
    val l: List[Int] = List(1, 2, 3)
    val o: Option[List[Int]] =
      Traverse.listApp.traverse[Option, Int, Int](l)(Some(_))
    o shouldBe Some(List(1, 2, 3))
  }
}
