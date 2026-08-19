package com.myway.traverse

case class Tree[A](e: Either[(Tree[A], Tree[A]), A]) {

  def fold[B](f: A => B, op: B => B => B): B = e match {
    case Right(a)     => f(a)
    case Left((l, r)) => op(l.fold(f, op))(r.fold(f, op))
  }

  def trace: String = fold[String](_.toString, s1 => s2 => s"[$s1.$s2]")

}

object Tree {

  def tip[A](a: A) = Tree(Right(a))
  def bin[A](l: Tree[A], r: Tree[A]) = Tree(Left((l, r)))
}
