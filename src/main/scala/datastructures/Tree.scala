package datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l).max(maximum(r))
  }

  def depth[A](r: Tree[A]): Int = r match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + depth(l).max(depth(r))
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

//  def fold[A,B](t: Tree[A], acc: B)(f: (B, A) => B): B = t match {
//    case Leaf(v) => f(acc, v)
//    case Branch(l, r) => fold(r, fold(l, acc)(f))
//  }
}