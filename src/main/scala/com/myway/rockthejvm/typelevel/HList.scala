package com.myway.rockthejvm.typelevel

trait HList

class HNil extends HList

class ::[H <: Nat, T <: HList] extends HList

trait Split[HL <: HList, L <: HList, R <: HList]

object Split {

  implicit val basic: Split[HNil, HNil, HNil] = new Split[HNil, HNil, HNil] {}

  implicit def basic2[N <: Nat]: Split[N :: HNil, N :: HNil, HNil] =
    new Split[N :: HNil, N :: HNil, HNil] {}

  implicit def inductive[
      N1 <: Nat,
      N2 <: Nat,
      T <: HList,
      L <: HList,
      R <: HList
  ](implicit
      split: Split[T, L, R]
  ): Split[N1 :: N2 :: T, N1 :: L, N2 :: R] =
    new Split[N1 :: N2 :: T, N1 :: L, N2 :: R] {}

  def apply[HL <: HList, L <: HList, R <: HList](implicit split: Split[HL,L,R]): Split[HL, L, R] = split


}
