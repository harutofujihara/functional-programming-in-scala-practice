package applicative

import monados._
import monados.Functor
import monoids._

import state._
import State._
import monoids._
import language.higherKinds
import language.implicitConversions

trait Applicative[F[_]] extends Functor[F] {
  // def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  // `map2` is implemented by first currying `f` so we get a function
  // of type `A => B => C`. This is a function that takes `A` and returns
  // another function of type `B => C`. So if we map `f.curried` over an
  // `F[A]`, we get `F[B => C]`. Passing that to `apply` along with the
  // `F[B]` will give us the desired `F[C]`.
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)(f.curried))(fb)

  // We simply use `map2` to lift a function into `F` so we can apply it
  // to both `fab` and `fa`. The function being lifted here is `_(_)`,
  // which is the same as the lambda notation `(f, x) => f(x)`. That is,
  // It's a function that takes two arguments:
  //   1. A function `f`
  //   2. An argument `x` to that function
  // and it simply applies `f` to `x`.
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)(_(_))

  // def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]
  def unit[A](a: => A): F[A]

  // def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
  //   apply(unit(f))(product(fa, fb))

  // def map[A, B](fa: F[A])(f: A => B): F[B]
  //   map2(fa, unit())((a, _) => f(a))

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(fa => fa)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  // def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
  //   map2(fa, fb)((_, _))
  def product[G[_]](
      G: Applicative[G]
  ): Applicative[({ type f[x] = (F[x], G[x]) })#f] = {
    val self = this
    new Applicative[({ type f[x] = (F[x], G[x]) })#f] {
      def unit[A](a: => A) = (self.unit(a), G.unit(a))
      override def apply[A, B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])) =
        (self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))
    }
  }

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(map(fa)(f.curried))(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(
      f: (A, B, C, D) => E
  ): F[E] =
    apply(apply(apply(map(fa)(f.curried))(fb))(fc))(fd)

  // def product[G[_]](
  //     G: Applicative[G]
  // ): Applicative[({ type f[x] = (F[x], G[x]) })#f] =

  def compose[G[_]](
      G: Applicative[G]
  ): Applicative[({ type f[x] = F[G[x]] })#f] = {
    val self = this
    new Applicative[({ type f[x] = F[G[x]] })#f] {
      def unit[A](a: => A) = self.unit(G.unit(a))
      override def apply[A, B](fab: F[G[A => B]])(fa: F[G[A]]): F[G[B]] =
        self.map2(fab, fa)((gab, ga) => G.apply(gab)(ga))
    }
  }

  // def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
  //   ofa.foldRight(scala.collection.mutable.Map[K, V]())()

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    (ofa foldLeft unit(Map.empty[K, V])) { case (acc, (k, fv)) =>
      map2(acc, fv)((m, v) => m + (k -> v))
    }
}

object Applicative {
  val streamApplicative = new Applicative[Stream] {
    def unit[A](a: => A): Stream[A] = Stream.continually(a)

    override def map2[A, B, C](a: Stream[A], b: Stream[B])(
        f: (A, B) => C
    ): Stream[C] = a.zip(b).map(f.tupled)
  }

  def validationApplicative[E] =
    new Applicative[({ type f[x] = Validation[E, x] })#f] {
      def unit[A](a: => A): Validation[E, A] = Success(a)

      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(
          f: (A, B) => C
      ): Validation[E, C] = (fa, fb) match {
        case (Success(a), Success(b)) => Success(f(a, b))
        case (Failure(eh1, et1), Failure(eh2, et2)) =>
          Failure(eh1, et1 ++ (eh2 +: et2))
        case (Failure(eh, et), _) => Failure(eh, et)
        case (_, Failure(eh, et)) => Failure(eh, et)
      }
    }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = M.zero
      override def apply[A, B](m1: M)(m2: M): M = M.op(m1, m2)
    }
}

object Monad {
  def eitherMonad[E] =
    new Monad[({ type f[x] = Either[E, x] })#f] {
      def unit[A](a: => A): Either[E, A] = Right(a)
      override def flatMap[A, B](
          ma: Either[E, A]
      )(f: A => Either[E, B]): Either[E, B] = ma match {
        case Right(value) => f(value)
        case Left(e)      => Left(e)
      }
    }
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector())
    extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

trait Traverse[F[_]] {
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))
  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  def map[A, B](fa: F[A])(f: A => B): F[B]
}

case class Tree[+A](head: A, tail: List[Tree[A]])

object Traverse {
  // val listTraverse = new Traverse[List] {
  //   override def traverse[G[_]: Applicative, A, B](fa: List[A])(
  //       f: A => G[B]
  //   ): G[List[B]] =
  //     fa.foldRight(())((a, b) => f(a))
  // }
  val listTraverse = new Traverse[List] {
    override def traverse[G[_], A, B](as: List[A])(f: A => G[B])(implicit
        G: Applicative[G]
    ): G[List[B]] =
      as.foldRight(G.unit(List[B]()))((a, fbs) => G.map2(f(a), fbs)(_ :: _))
  }

  val optionTraverse = new Traverse[Option] {
    override def traverse[G[_], A, B](
        oa: Option[A]
    )(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
      oa match {
        case Some(a) => G.map(f(a))(Some(_))
        case None    => G.unit(None)
      }
  }

  val treeTraverse = new Traverse[Tree] {
    override def traverse[G[_], A, B](
        ta: Tree[A]
    )(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] =
      G.map2(f(ta.head), listTraverse.traverse(ta.tail)(a => traverse(a)(f)))(
        Tree(_, _)
      )
  }

  //
  //

}
