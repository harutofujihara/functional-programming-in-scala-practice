package laziness

import Stream._

import scala.annotation.tailrec
trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => Some(h())
  }

//  def toList: List[A] = this match {
//    case Empty => Nil
//    case Cons(h, t) => h() :: toList(t)
//  }
  def toListRecursive: List[A] = this match {
    case Cons(h, t) => h() :: t().toListRecursive
    case _          => List()
  }
  /*
  The above solution will stack overflow for large streams, since it's
  not tail-recursive. Here is a tail-recursive implementation. At each
  step we cons onto the front of the `acc` list, which will result in the
  reverse of the stream. Then at the end we reverse the result to get the
  correct order again.
   */
  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _          => acc
    }
    go(this, List()).reverse
  }
//  def take(n: Int): List[A] = this match {
//    case Cons(h, t) if (n <= 1) => List(h())
//    case Cons(h, t) => h() :: t().take(n - 1)
//    case _ => List()
//  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _                    => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n < 1) => this
    case Cons(h, t)            => t().drop(n - 1)
    case _                     => Empty
  }

  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if f(h()) => cons(h(), t() takeWhile f)
    case _                    => empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _          => z
  }

//  def forAll(p: A => Boolean): Boolean =
//    foldRight(true)((a, b) => b && p(a))
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => {
      p(a) && {
//        println(a)
        b
      }
    })

//  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
//    foldRight(empty[A])((a, b) => if(p(a)) cons(a, b) else b)
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

  def headOptionViaFoldRight: Option[A] =
    foldRight(None: Option[A])((a, b) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty: Stream[B])((h, t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)

  def append[AA >: A](s: Stream[AA]): Stream[AA] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty: Stream[B])((h, t) => f(h).append(t))

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this)(s =>
      s match {
        case Cons(h, t) => Some((f(h()), t()))
        case _          => None
      }
    )

  def takeViaUnfold(n: Int): Stream[A] =
    unfold(this) {
      case Cons(h, t) if 0 < n => Some((h(), t().takeViaUnfold(n - 1)))
      case _                   => None
    }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
    unfold(this) {
//      case Cons(h, t) if (f(h())) => Some((h(), t().takeWhileViaUnfold(f)))
      case Cons(h, t) if (f(h())) => Some((h(), t()))
      case _                      => None
    }

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s)) {
      case (Cons(h, t), Cons(hh, tt)) => Some((f(h(), hh()), (t(), tt())))
      case _                          => None
    }
  // special case of `zipWith`
  def zip[B](s2: Stream[B]): Stream[(A, B)] =
    zipWith(s2)((_, _))

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Cons(h, t), Cons(hh, tt)) =>
        Some(((Some(h()), Some(hh())), (t(), tt())))
      case (Cons(h, t), _) => Some(((Some(h()), None), (t(), empty)))
      case (_, Cons(h, t)) => Some(((None, Some(h())), (empty, t())))
      case _               => None
    }

//  def startsWith[A](s: Stream[A]): Boolean =
//    zipAll(s).forAll {
//      case (s1, s2) => (s1.nonEmpty && s2.isEmpty) || (s1.nonEmpty && s2.nonEmpty && s1.get == s2.get)
//    }
  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forAll { case (h, h2) =>
      h == h2
    }

//  def tails: Stream[Stream[A]] =
//    unfold(this) {
//      case Cons(h, t) => Some(cons(h(), t()), t())
//      case _ => None
//    }
  /*
  The last element of `tails` is always the empty `Stream`, so we handle this as a special case, by appending it to the output.
   */
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s     => Some((s, s drop 1))
    } append Stream(empty)

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    foldRight(cons(z, empty))((a, b) => cons(f(a, b.headOption.get), b))

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs(): Stream[Int] = {
    def go(p1: Int, p2: Int): Stream[Int] = {
      lazy val cur = p1 + p2
      cons(cur, go(cur, p1))
    }
    cons(0, go(0, 1))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z)
      .map { case (a, s) =>
        cons(a, unfold(s)(f))
      }
      .getOrElse(empty)

  val fibsViaUnfold = {
    unfold((0, 1)) { case (i1, i2) =>
      Some((i1, (i2, i1 + i2)))
    }
  }

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(s => Some((s, s + 1)))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(aa => Some(aa, aa))

  val ones: Stream[Int] = unfold(1)(i => Some(1, 1))

}
