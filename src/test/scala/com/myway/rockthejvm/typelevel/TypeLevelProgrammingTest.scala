package com.myway.rockthejvm.typelevel

import org.scalatest.funsuite.AsyncFunSuite
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class TypeLevelProgrammingTest extends AsyncFunSuite {

  test("show ") {
    TypeLevelProgramming.show(List(1, 2, 4)) shouldBe "TypeTag[List[Int]]"
  }

}
