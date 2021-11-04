package errorhandling

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }
  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None: Option[B])
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)).getOrElse(ob)
  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
//  def variance(xs: Seq[Double]): Option[Double] = Some(xs).flatMap(s =>
//    if (d.isEmpty) None
//    else {
//      val m = s.sum / s.length
//      s.foldLeft(0)((acc, x) => acc + scala.math.pow(x - m, 2)) / s.length
//    }
//  )

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(av => b.map(bv => f(av, bv)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight(Some(List[A]()): Option[List[A]])((aa, b) => b.flatMap(bb => aa.map(aaa => aaa +: bb)))

  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(Nil): Option[List[B]])((aa, b) => map2(f(aa), b)(_ :: _))
}
