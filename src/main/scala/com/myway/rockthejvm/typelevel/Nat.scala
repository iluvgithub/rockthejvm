package com.myway.rockthejvm.typelevel

import com.myway.rockthejvm.typelevel.Nat._

trait Nat
class _0 extends Nat
class Succ[N <: Nat] extends Nat

object Nat {

  type _1 = Succ[_0]
  type _2 = Succ[_1]
  type _3 = Succ[_2]
  type _4 = Succ[_3]
}

trait <[A <: Nat, B <: Nat]
object < {
  implicit def ltBasic[B <: Nat]: <[_0, Succ[B]] = new <[_0, Succ[B]] {}
  def apply[A <: Nat, B <: Nat](implicit lt: <[A, B]): A < B = lt

  val comparison01: <[_0, _1] = <[_0, _1]
  val comparison02: _0 < _2 = <[_0, _2]

  implicit def inductive[A <: Nat, B <: Nat](implicit lt: <[A, B]) =
    new <[Succ[A], Succ[B]] {}
  val comparison12: <[_1, _2] = <[_1, _2]
  val comparison13: <[_1, _3] = <[_1, _3]

}

trait <=[A <: Nat, B <: Nat]
object <= {
  implicit def leBasic[B <: Nat]: <=[_0, Succ[B]] = new <=[_0, Succ[B]] {}
  def apply[A <: Nat, B <: Nat](implicit le: <=[A, B]): A <= B = le

  implicit def inductive[A <: Nat, B <: Nat](implicit
      le: <=[A, B]
  ): Succ[A] <= Succ[B] =
    new <=[Succ[A], Succ[B]] {}

  val comparison13: <=[_1, _3] = <=[_1, _3]
}
