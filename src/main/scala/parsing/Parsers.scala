package parsing

import scala.util.matching.Regex
import language.implicitConversions

trait Parsers[ParseError, Parser[+_]] { self =>
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit
      f: A => Parser[String]
  ): ParserOps[String] = ParserOps(f(a))
  implicit def regex(r: Regex): Parser[String]

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    p.flatMap(a => p2.map(b => (a, b)))

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    n match {
      case i if i <= 0 => succeed(Nil)
      case ii          => map2(p, listOfN(ii - 1, p))(_ :: _)
    }

  def map[A, B](a: Parser[A])(f: A => B): Parser[B] =
    a.flatMap(aa => succeed(f(aa)))
  // def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
  //   (p ** p2).map { case (a, b) => f(a, b) }
  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    p.flatMap(a => p2.map(b => f(a, b)))

  // def many[A](p: Parser[A]): Parser[List[A]] =?
  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())
  // def many1[A](p: Parser[A]): Parser[List[A]] = ?
  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def succeed[A](a: A): Parser[A] = string("").map(_ => a)

  def slice[A](p: Parser[A]): Parser[String]

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def many = self.many(p)

    def **[B](p2: => Parser[B]): Parser[(A, B)] =
      self.product(p, p2)
    def product[B](p2: => Parser[B]): Parser[(A, B)] =
      self.product(p, p2)

    def flatMap[B](f: A => Parser[B]): Parser[B] =
      self.flatMap(p)(f)
  }

  object Laws {
    import testing._
    import testing.Prop._
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
  }
}
