package com.myway.rockthejvm.typelevel

import com.myway.rockthejvm.typelevel.Nat._

// https://www.youtube.com/watch?v=qwUYqv6lKtQ&list=PLmtsMNDRU0ByOQoz6lnihh6CtMrErNax7
trait Nat

class _0 extends Nat

class Succ[N <: Nat] extends Nat

object Nat {

  type _1 = Succ[_0]
  type _2 = Succ[_1]
  type _3 = Succ[_2]
  type _4 = Succ[_3]
  type _5 = Succ[_4]
}

trait <[A <: Nat, B <: Nat]

object < {
  implicit def ltBasic[B <: Nat]: <[_0, Succ[B]] = new <[_0, Succ[B]] {}

  def apply[A <: Nat, B <: Nat](implicit lt: <[A, B]): A < B = lt

  val comparison01: <[_0, _1] = <[_0, _1]
  val comparison02: _0 < _2 = <[_0, _2]
  val comparison03: _0 < _3 = <[_0, _3]
  val comparison04: _0 < _4 = <[_0, _4]

  implicit def inductive[A <: Nat, B <: Nat](implicit
      lt: <[A, B]
  ): Succ[A] < Succ[B] =
    new <[Succ[A], Succ[B]] {}

  val comparison12: _1 < _2 = <[_1, _2]
  val comparison13: _1 < _3 = <[_1, _3]
  val comparison14: _1 < _4 = <[_1, _4]

}

trait <=[A <: Nat, B <: Nat]

object <= {

  implicit def leZero[B <: Nat]: <=[_0, _0] = new <=[_0, _0] {}
  implicit def leBasic[B <: Nat]: <=[_0, Succ[B]] = new <=[_0, Succ[B]] {}

  def apply[A <: Nat, B <: Nat](implicit le: <=[A, B]): A <= B = le

  implicit def inductive[A <: Nat, B <: Nat](implicit
      le: <=[A, B]
  ): Succ[A] <= Succ[B] =
    new <=[Succ[A], Succ[B]] {}

  val comparison00: <=[_0, _0] = <=[_0, _0]
  val comparison13: <=[_1, _3] = <=[_1, _3]
  val comparison22: <=[_2, _2] = <=[_2, _2]
}

trait +[A <: Nat, B <: Nat, S <: Nat] // https://www.youtube.com/watch?v=EGAJJpGODHg&list=PLmtsMNDRU0ByOQoz6lnihh6CtMrErNax7&index=2

object + {

  implicit val zero: +[_0, _0, _0] = new +[_0, _0, _0] {}

  implicit def basicRight[A <: Nat](implicit lt: _0 < A): +[_0, A, A] =
    new +[_0, A, A] {}
  implicit def basicLeft[A <: Nat](implicit lt: _0 < A): +[A, _0, A] =
    new +[A, _0, A] {}

  val oneL: +[_0, _1, _1] = +.apply
  val oneR: +[_1, _0, _1] = +.apply

  def apply[A <: Nat, B <: Nat, S <: Nat](implicit
      plus: +[A, B, S]
  ): +[A, B, S] = plus

  implicit def inductive[A <: Nat, B <: Nat, S <: Nat](implicit
      plus: +[A, B, S]
  ): +[Succ[A], Succ[B], Succ[Succ[S]]] =
    new +[Succ[A], Succ[B], Succ[Succ[S]]] {}

  import com.myway.rockthejvm.typelevel.Nat.{_1, _3, _4}
  val fourL: +[_1, _3, _4] = +[_1, _3, _4]
  val fourR: +[_3, _1, _4] = +[_3, _1, _4]
}
