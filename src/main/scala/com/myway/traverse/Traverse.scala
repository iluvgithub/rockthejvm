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

  implicit def treeApp: Traverse[Tree] = new Traverse[Tree] {
    override def traverse[M[_], A, B](
        t: Tree[A]
    )(g: A => M[B])(implicit AP: Applicative[M]): M[Tree[B]] = {
      t.fold[M[Tree[B]]](
        x => AP.ap(AP.pure(x => Tree.tip[B](x)))(g(x)),
        l =>
          r =>
            AP.ap(
              AP.ap(
                AP.pure[Tree[B] => Tree[B] => Tree[B]](l =>
                  r => Tree.curryBin[B](l)(r)
                )
              )(l)
            )(r)
      )

    }
  }

  implicit def listApp: Traverse[List] = new Traverse[List] {

    override def traverse[M[_], A, B](
        t: List[A]
    )(g: A => M[B])(implicit AP: Applicative[M]): M[List[B]] =
      t.map(g)
        .foldRight[M[List[B]]](AP.pure(Nil))((a, acc) =>
          AP.map(AP.product(acc, a))({ case (xs, x) => x :: xs })
        )
  }
}
