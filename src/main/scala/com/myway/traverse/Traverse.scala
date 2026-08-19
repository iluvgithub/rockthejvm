package com.myway.traverse

import cats.Applicative

trait Traverse[T[_]] {

  def traverse[M[_], A, B](t: T[A])(g: A => M[B])(implicit
      AP: Applicative[M]
  ): M[T[B]]

  def sequence[M[_], A](t: T[M[A]])(implicit AP: Applicative[M]): M[T[A]] =
    traverse(t)(identity)

}

object Traverse {

  implicit def treeApp: Applicative[Tree] = new Applicative[Tree] {
    override def pure[A](a: A): Tree[A] = Tree.tip(a)

    override def ap[A, B](ff: Tree[A => B])(fa: Tree[A]): Tree[B] =
      fa.fold[Tree[B]](
        ???,
        ???
      )
  }

}
