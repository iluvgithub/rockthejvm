package com.myway.rockthejvm.typelevel

import com.myway.rockthejvm.typelevel.Nat.{_1, _2, _3, _4}
import com.myway.rockthejvm.typelevel.TypeLevelProgramming._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class NatTest extends AnyFunSuite with Matchers {

  test("Peano < ") {

    import com.myway.rockthejvm.typelevel.<._
    val comparison13: <[_2, _3] = com.myway.rockthejvm.typelevel.<[_2, _3]
    // val comparison23: <[_3, _2] = com.myway.rockthejvm.typelevel.<[_3, _2]
    show(comparison13) shouldBe "TypeTag[._2 < ._3]"
  }

  test("Peano <= ") {
    import com.myway.rockthejvm.typelevel.<=._
    val comparison24: <=[_2, _4] = com.myway.rockthejvm.typelevel.<=[_2, _4]
    show(comparison24) shouldBe "TypeTag[._2 <= ._4]"

    val comparison00: <=[_0, _0] = com.myway.rockthejvm.typelevel.<=[_0, _0]
    val comparison11: <=[_1, _1] = com.myway.rockthejvm.typelevel.<=[_1, _1]
  }
}
