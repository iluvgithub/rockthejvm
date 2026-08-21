package com.myway.traverse

import cats.Applicative

trait Traverse[T[_]] {

  def traverse[M[_], A, B](
      t: T[A]
  )(
      g: A => M[B]
  )(implicit
      AP: Applicative[M]
  ): M[T[B]]

  def sequence[M[_], A](
      t: T[M[A]]
  )(implicit
      AP: Applicative[M]
  ): M[T[A]] =
    traverse(t)(identity)

  def traverseBack[M[_], A, B](
      t: T[A]
  )(
      g: A => M[B]
  )(implicit
      AP: Applicative[M]
  ): M[T[B]] = {

    type G[X] = Backwards[M, X]

    val q: A => G[B] =
      x => Backwards[M, B](g(x))

    val z: G[T[B]] =
      traverse[G, A, B](t)(q)

    z.forwards
  }
}
object Traverse {
  def backTraverse[F[_], M[_], A, B](
      f: A => M[B]
  )(
      fa: F[A]
  )(implicit
      F: Traverse[F],
      M: Applicative[M]
  ): M[F[B]] =
    F.traverseBack(fa)(f)

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
