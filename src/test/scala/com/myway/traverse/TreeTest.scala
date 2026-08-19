package com.myway.traverse

import com.myway.traverse.Tree.{bin, tip}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TreeTest extends AnyFunSuite with Matchers {

  val tr: Tree[Char] = bin(tip('a'), bin(bin(tip('b'), tip('c')), tip('d')))

  test("fold ") {
    tr.trace shouldBe "[a.[[b.c].d]]"
  }

  test("map trace ") {
    tr.map(_.toUpper).trace shouldBe "[A.[[B.C].D]]"
  }

}
