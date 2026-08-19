package com.myway.traverse

import cats.Applicative

trait Traverse[T[_]] {

  def traverse[M[_], A, B](t: T[A])(g: A => M[B])(implicit
      AP: Applicative[M]
  ): M[T[B]]

  def sequence[M[_], A](t: T[M[A]])(implicit AP: Applicative[M]): M[T[A]] =
    traverse(t)(identity)

}
