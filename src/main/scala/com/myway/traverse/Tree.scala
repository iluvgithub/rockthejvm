package com.myway.traverse

import cats.Applicative
import cats.data.State

case class Tree[A](e: Either[(Tree[A], Tree[A]), A]) {

  def fold[B](f: A => B, op: B => B => B): B = e match {
    case Right(a)     => f(a)
    case Left((l, r)) => op(l.fold(f, op))(r.fold(f, op))
  }

  def trace: String = fold[String](_.toString, s1 => s2 => s"[$s1.$s2]")

  def map[B](f: A => B): Tree[B] =
    fold[Tree[B]](f andThen Tree.tip, Tree.curryBin)

  def traverse[M[_], B](g: A => M[B])(implicit AP: Applicative[M]) =
    Traverse.treeApp.traverse(this)(g)
}

object Tree {

  def tip[A](a: A) = Tree(Right(a))
  def bin[A](l: Tree[A], r: Tree[A]) = Tree(Left((l, r)))
  def curryBin[A](l: Tree[A])(r: Tree[A]): Tree[A] = bin(l, r)

  def label[A, B](t: Tree[A]): State[List[B], Tree[(A, B)]] =
    t.traverse(adorn[A, B])

  def adorn[A, B](a: A): State[List[B], (A, B)] = for {
    bs <- State.get
    _ <- State.set(bs.tail)
  } yield (a, bs.head)

  def labelize[A, B](t: Tree[A]): List[B] => Tree[(A, B)] = bs =>
    label(t).runA(bs).value

  def strip[A, B](a: A, b: B): State[List[B], A] = for {
    bs <- State.get
    _ <- State.set(b :: bs)
  } yield a

  def unlabel[A, B]: Tree[(A, B)] => State[List[B], Tree[A]] =
    Traverse.treverse[Tree, ({ type l[X] = State[List[B], X] })#l, (A, B), A](
      x => strip(x._1, x._2)
    )

}
