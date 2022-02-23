package monados

import state._

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] =
    e match {
      case Left(fa)  => map(fa)(Left(_))
      case Right(fb) => map(fb)(Right(_))
    }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as.map(f)
  }
}

trait Monad[F[_]] {
  def unit[A](a: => A): F[A]
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  // def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] =
    compose((_: Unit) => ma, f)(())

  def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def flatMapViaJoinAndMap[A, B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(f))

  // def sequence[A](lma: List[F[A]]): F[List[A]] = lma match {
  //   case head :: tl => flatMap(head)(a => map(sequence(tl))(a :: _))
  //   case Nil        => unit(List())
  // }

  // def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
  //   sequence(la.map(f))
  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List[A]()))((ma, mla) => map2(ma, mla)(_ :: _))

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List[B]()))((a, mlb) => map2(f(a), mlb)(_ :: _))

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  // def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms match {
      case Nil => unit(Nil)
      case h :: t =>
        flatMap(f(h))(b =>
          if (!b) filterM(t)(f)
          else map(filterM(t)(f))(h :: _)
        )
    }
}

import testing._
import parallelism._
import parallelism.Par.Par
import parsing._
import laziness._
import datastructures._

object Monado {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] = ma.flatMap(f)
  }

  val ParMonad = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)
    def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = flatMap(ma)(f)
  }

  // val ParserMonad = new Monad[Parsers] {
  //   //
  // }
  def parserMonad[P[+_]](p: Parsers[_, P]) = new Monad[P] {
    def unit[A](a: => A) = p.succeed(a)
    def flatMap[A, B](ma: P[A])(f: A => P[B]) = p.flatMap(ma)(f)
  }

  val OptionMonad = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)
    def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] =
      ma.flatMap(f)
  }

  val StreamMonad = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)
    def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] =
      ma.flatMap(f)
  }

  val ListMonad: Monad[List] = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)
    def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = flatMap(ma)(f)
  }

  case class Id[A](value: A) {
    def flatMap[B](f: A => Id[B]): Id[B] = f(value)
    def map[B](f: A => B): Id[B] = Id(f(value))
  }

  val IdMonad = new Monad[Id] {
    def unit[A](a: => A): Id[A] = Id(a)
    def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma.flatMap(f)
  }

  def stateMonado[S] = new Monad[({ type f[x] = State[S, x] })#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st.flatMap(f)
  }

  case class Reader[R, A](run: R => A)

  // object Reader {
  //   def readerMonad[R] = new Monad[({ type f[x] = Reader[R, x] })#f] {
  //     def unit[A](a: => A): Reader[R, A] = Reader(r => a)
  //     def flatMap[A, B](ma: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B]

  //   }
  // }
  object Reader {
    def readerMonad[R] = new Monad[({ type f[x] = Reader[R, x] })#f] {
      def unit[A](a: => A): Reader[R, A] = Reader(_ => a)
      def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
        Reader(r => f(st.run(r)).run(r))
    }

    // A primitive operation for it would be simply to ask for the `R` argument:
    def ask[R]: Reader[R, R] = Reader(r => r)
  }
}
