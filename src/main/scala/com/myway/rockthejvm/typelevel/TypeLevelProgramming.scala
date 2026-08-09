package com.myway.rockthejvm.typelevel

object TypeLevelProgramming {

  import scala.reflect.runtime.universe._
  def show[T](v: T)(implicit tag: TypeTag[T]) = tag
    .toString()
    .replace(
      "com.myway.rockthejvm.typelevel.TypeLevelProgramming.",
      ""
    )
    .replace(
      "com.myway.rockthejvm.typelevel.Nat.",
      ""
    )
    .replace(
      "com.myway.rockthejvm.typelevel.",
      ""
    )
}
