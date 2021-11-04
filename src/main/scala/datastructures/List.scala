package datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_,t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = n match {
    case m if m < 0 => sys.error("n should be positive.")
    case 0 => l
    case m => l match {
      case Nil => sys.error("drop of empty list")
      case Cons(x, xs) => drop(xs, n - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(h,t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, acc) => acc + 1)

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((b, a) => a match {
      case Nil => b
      case x => Cons(x, b)
    })

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((a, b) => a match {
      case Nil => b
      case x => Cons(x, b)
    })

  def flat[A](l: List[List[A]]): List[A] =
    foldRight(l, List[A]())((a, b) => append2(a,b))

  def addOne(l: List[Int]): List[Int] =
    foldRight(l, List[Int]())((a, b) => Cons(a + 1, b))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, List[String]())((a, b) => Cons(a.toString, b))

  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((h, t) => Cons(f(h), t))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, List[A]())((h, t) => if (f(h)) Cons(h, t) else t)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append2)

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def plusEachElement(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Cons(h, t), Cons(hh, tt)) => Cons(h + hh, plusEachElement(t, tt))
    case (x, Nil) => x
    case (Nil, x) => x
    case _ => Nil
  }

//  def zipWith[A, B](l1: List[A], l2: List[A])(f: (A, A) => B): List[B] = (l1, l2) match {
//    case (Nil, _) => Nil
//    case (_, Nil) => Nil
//    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
//  }
def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = (a,b) match {
  case (Nil, _) => Nil
  case (_, Nil) => Nil
  case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
}

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def startsWith(p: List[A], b: List[A]): Boolean = (p, b) match {
      case (Cons(h1, _), Cons(h2, Nil)) if (h1 == h2) => true
      case (Cons(h1, t1), Cons(h2, t2)) if (h1 == h2) => startsWith(t1, t2)
      case _ => false
    }

    (sup, sub) match {
      case (Cons(h1, t1), Cons(_, t2)) => startsWith(sup, sub) || hasSubsequence(t1, sub)
      case _ => false
    }
  }
}
