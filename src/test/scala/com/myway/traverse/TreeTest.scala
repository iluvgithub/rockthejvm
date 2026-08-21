package com.myway.traverse

import cats.data.State
import com.myway.traverse.Tree.{bin, writeBck, writeFwd, tip}
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

  test("traverse") {
    val o: Option[Tree[Char]] =
      Traverse.treeApp.traverse[Option, Char, Char](tr)(Some(_))
    o.map(_.trace) shouldBe Some("[a.[[b.c].d]]")
  }

  test("label ") {
    val l = List(1, 2, 3, 4, 5)
    Tree.labelling(tr)(l).trace shouldBe "[(a,1).[[(b,2).(c,3)].(d,4)]]"
  }

  test("unlabel ") {
    val st: State[List[Int], Tree[(Char, Int)]] = Tree.label[Char, Int](tr)

    val state: State[List[Int], Tree[Char]] = for {
      tr2 <- st
      t <- Tree.unlabel[Char, Int](tr2)
    } yield t

    val ti: Tree[Char] = state.runA(List(5, 6, 7, 8)).value
    ti.trace shouldBe "[a.[[b.c].d]]"
  }

  test("writer fwd / back") {
    val fwd: List[Char] = writeBck(tr)
    val bak: List[Char] = writeFwd(tr)

    fwd shouldBe List('d', 'c', 'b', 'a')
    bak shouldBe List('a', 'b', 'c', 'd')
  }

}
