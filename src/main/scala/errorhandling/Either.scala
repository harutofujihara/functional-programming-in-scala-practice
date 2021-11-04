package errorhandling

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(value) => Left(value)
    case Right(value) => Right(f(value))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    flatMap(a => b.map(bb => f(a, bb)))
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es.foldRight(Right(List[A]()): Either[E, List[A]])((a, b) => a.map2(b)((aa, bb) => aa +: bb))

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight(Right(List[B]()): Either[E, List[B]])((a, b) => f(a).map2(b)(_ :: _))
}