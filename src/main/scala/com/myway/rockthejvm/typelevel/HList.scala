package com.myway.rockthejvm.typelevel

import com.myway.rockthejvm.typelevel.Nat.{_1, _2, _3, _4, _5}

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

  def apply[HL <: HList, L <: HList, R <: HList](implicit
      split: Split[HL, L, R]
  ): Split[HL, L, R] = split

}

trait Merge[LA <: HList, LB <: HList, L <: HList]

object Merge {
  implicit def basicLeft[L <: HList]: Merge[HNil, L, L] =
    new Merge[HNil, L, L] {}

  implicit def basicRight[L <: HList]: Merge[L, HNil, L] =
    new Merge[L, HNil, L] {}

  implicit def indictiveLte[
      N1 <: Nat,
      T1 <: HList,
      N2 <: Nat,
      T2 <: HList,
      IR <: HList
  ](implicit
      merge: Merge[T1, N2 :: T2, IR],
      lte: N1 <= N2
  ): Merge[N1 :: T1, N2 :: T2, N1 :: IR] =
    new Merge[N1 :: T1, N2 :: T2, N1 :: IR] {}

  implicit def indictiveGte[
      N1 <: Nat,
      T1 <: HList,
      N2 <: Nat,
      T2 <: HList,
      IR <: HList
  ](implicit
      merge: Merge[N1 :: T1, T2, IR],
      gt: N2 < N1
  ): Merge[N1 :: T1, N2 :: T2, N2 :: IR] =
    new Merge[N1 :: T1, N2 :: T2, N2 :: IR] {}

  def apply[LA <: HList, LB <: HList, L <: HList](implicit
      merge: Merge[LA, LB, L]
  ): Merge[LA, LB, L] = merge

  val validMerge: Merge[
    _1 :: _3 :: HNil,
    _2 :: _4 :: HNil,
    _1 :: _2 :: _3 :: _4 :: HNil
  ] =
    Merge.apply
}

trait Sort[L <: HList] { type Result <: HList }

object Sort {

  type SortOp[L <: HList, R <: HList] = Sort[L] { type Result = R }

  implicit val basicNil: SortOp[HNil, HNil] = new Sort[HNil] {
    type Result = HNil
  }
  implicit def basicOne[N <: Nat]: Sort[N :: HNil] = new Sort[N :: HNil] {
    type Result = N :: HNil
  }

  implicit def inductive[
      I <: HList,
      L <: HList,
      R <: HList,
      SL <: HList,
      SR <: HList,
      O <: HList
  ](implicit
      split: Split[I, L, R],
      sortLeft: SortOp[L, SL],
      sortRight: SortOp[R, SR],
      merge: Merge[SL, SR, O]
  ): SortOp[I, O] = new Sort[I] { type Result = O }

  def apply[L <: HList](implicit sort: Sort[L]): SortOp[L, sort.Result] = sort

  val validSort: Sort[_1 :: HNil] = Sort[_1 :: HNil]

}
