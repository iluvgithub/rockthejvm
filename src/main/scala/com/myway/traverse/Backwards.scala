package com.myway.traverse

import cats.Applicative
import cats.syntax.all._

final case class Backwards[F[_], A](forwards: F[A])

object Backwards {


  implicit def applicativeInstance[F[_]](implicit
      F: Applicative[F]
  ): Applicative[({ type L[A] = Backwards[F, A] })#L] =
    new Applicative[({ type L[A] = Backwards[F, A] })#L] {

      override def pure[A](x: A): Backwards[F, A] =
        Backwards(F.pure(x))

      override def ap[A, B](
          ff: Backwards[F, A => B]
      )(fa: Backwards[F, A]): Backwards[F, B] =
        Backwards(
          fa.forwards.map2(ff.forwards)((x, f) => f(x))
        )
    }
}
