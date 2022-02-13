package monoids

import testing._
import parallelism.Par

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    val zero = 0
  }
  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2
    val zero = 1
  }
  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    val zero = false
  }
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    val zero = true
  }

  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = (a1, a2) match {
      case (a: Some[A], _) => a
      case (_, a: Some[A]) => a
      case _               => None
    }
    val zero = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A = a1.compose(a2)
    val zero = a => a
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    Prop.forAll(gen)(a => m.op(a, m.zero) == a && m.op(m.zero, a) == a)

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    concatenate(as.map(f), m)

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (v.isEmpty) m.zero
    else if (v.size == 1) f(v.head)
    else {
      val left = foldMapV(v.slice(0, v.length / 2), m)(f)
      val right = foldMapV(v.slice(v.length / 2, v.length), m)(f)
      m.op(left, right)
    }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  // val wcMonoid: Monoid[WC] = new Monoid[WC] {
  //   def op(a1: WC, a2: WC): WC =
  //     (a1, a2) match {
  //       case (Stub(s1), Stub(s2))           => Stub(s1 + s2)
  //       case (Stub(s), Part(ls, words, rs)) =>
  //     }
  // }
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    // The empty result, where we haven't seen any characters yet.
    val zero = Stub("")

    def op(a: WC, b: WC) = (a, b) match {
      case (Stub(c), Stub(d))       => Stub(c + d)
      case (Stub(c), Part(l, w, r)) => Part(c + l, w, r)
      case (Part(l, w, r), Stub(c)) => Part(l, w, r + c)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
        Part(l1, w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2, r2)
    }
  }

  // def countWords(s: String): Int = {
  //   def loop(ss: String): WC = {
  //     if (ss.isEmpty) wcMonoid.zero
  //     else if (ss.length == 1) Stub(ss)
  //     else {
  //       val (l, r) = ss.splitAt(s.length / 2)
  //       wcMonoid.op(loop(l), loop(r))
  //     }
  //   }

  //   val m = loop(s)
  //   m match {
  //     case Part(lStub, words, rStub) =>
  //       words + (if (lStub.isEmpty) 0 else 1) + (if (rStub.isEmpty) 0 else 1)
  //     case Stub(chars) => if (chars.isEmpty) 0 else 1
  //   }
  // }
  def count(s: String): Int = {
    def wc(c: Char): WC =
      if (c.isWhitespace)
        Part("", 0, "")
      else
        Stub(c.toString)
    def unstub(s: String) = s.length min 1
    foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(s)       => unstub(s)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }
  }

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      def op(x: (A, B), y: (A, B)) =
        (A.op(x._1, y._1), B.op(x._2, y._2))
      val zero = (A.zero, B.zero)
    }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero = Map[K, V]()
      def op(a: Map[K, V], b: Map[K, V]) =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero), b.getOrElse(k, V.zero)))
        }
    }

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      def op(a1: A => B, a2: A => B): A => B = a => B.op(a1(a), a2(a))
      def zero: A => B = _ => B.zero
    }

  // def bag[A](as: IndexedSeq[A]): Map[A, Int]
  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))((a: A) => Map(a -> 1))

}

// trait Foldable[F[_]] {
//   def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
//   def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
//   def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
//   def concatenate[A](as: F[A])(m: Monoid[A]): A
// }

// object Foldable {
//   val listFoldable = new Foldable[List] {
//     def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as match {
//       case head :: tl => f(head, foldRight(tl)(z)(f))
//       case Nil        => z
//     }

//     def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as match {
//       case head :: tl => foldLeft(tl)(f(z, head))(f)
//       case Nil        => z
//     }

//     def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B = as match {
//       case head :: tl => mb.op(f(head), foldMap(tl)(f)(mb))
//       case Nil        => mb.zero
//     }

//     def concatenate[A](as: List[A])(m: Monoid[A]): A = as match {
//       case head :: tl => m.op(head, concatenate(tl)(m))
//       case Nil        => m.zero
//     }
//   }
// }

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())(_ :: _)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  import Monoid._
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    foldMapV(as, mb)(f)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
    as match {
      case Branch(left, right) => foldRight(left)(foldRight(right)(z)(f))(f)
      case Leaf(value)         => f(value, z)
    }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
    case Branch(left, right) => foldLeft(right)(foldLeft(left)(z)(f))(f)
    case Leaf(value)         => f(z, value)
  }

  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case Branch(left, right) =>
        mb.op(foldMap(left)(f)(mb), foldMap(right)(f)(mb))
      case Leaf(value) => f(value)
    }
}

object OptionFoldable extends Foldable[Option] {
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
    as match {
      case None        => z
      case Some(value) => f(value, z)
    }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
    as match {
      case None        => z
      case Some(value) => f(z, value)
    }

  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case None        => mb.zero
      case Some(value) => f(value)
    }
}
