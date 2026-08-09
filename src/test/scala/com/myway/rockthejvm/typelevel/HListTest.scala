package com.myway.rockthejvm.typelevel

import com.myway.rockthejvm.typelevel.Nat.{_1, _2, _3, _4, _5}
import com.myway.rockthejvm.typelevel.TypeLevelProgramming.show
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class HListTest extends AnyFunSuite with Matchers {

  test("valid split") {

    val validSplit
        : Split[_1 :: _2 :: _3 :: HNil, _1 :: _3 :: HNil, _2 :: HNil] =
      Split.apply
    show(
      validSplit
    ) shouldBe "TypeTag[Split[_1 :: _2 :: _3 :: HNil,_1 :: _3 :: HNil,_2 :: HNil]]"
  }

  test("merge") {
    val validMerge: Merge[
      _1 :: _3 :: HNil,
      _2 :: _4 :: HNil,
      _1 :: _2 :: _3 :: _4 :: HNil
    ] =
      Merge.apply

    show(
      validMerge
    ) shouldBe "TypeTag[Merge[_1 :: _3 :: HNil,_2 :: _4 :: HNil,_1 :: _2 :: _3 :: _4 :: HNil]]"
  }

  test("sort") {
    val validSort  = Sort[ _4 :: _3 :: _1 :: _2 :: _5 :: HNil,
      _1 :: _2 :: _3 :: _4 :: _5 :: HNil]
    show(
      validSort
    ) shouldBe "TypeTag[Sort[_4 :: _3 :: _1 :: _2 :: _5 :: HNil,_1 :: _2 :: _3 :: _4 :: _5 :: HNil]]"
  }
}
