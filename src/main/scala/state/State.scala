package state

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed =
        (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = SimpleRNG(
        newSeed
      ) // The next state, which is an `RNG` instance created from the new seed.
      val n =
        (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (
        n,
        nextRNG
      ) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

//  def double(rng: RNG): (Double, RNG) = {
//    val (i, r) = rng.nextInt
//    val a / (b + c)
//    (if (i < 0) (i + 1).toDouble / Int.MinValue else -(i.toDouble / Int.MinValue), r)
//  }
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = nonNegativeInt(rng)
    val (d, r2) = double(r)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r) = double(rng)
    val (i, r2) = nonNegativeInt(r)
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d, r) = double(rng)
    val (d2, r2) = double(r)
    val (d3, r3) = double(r2)
    ((d, d2, d3), r3)
  }

//  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
//    if (count < 1) return (Nil, rng)
//    else {
//      val (i, r) = nonNegativeInt(rng)
//      val (tails, r2) = ints(count - 1)(r)
//      (i +: tails, r2)
//    }
//  }
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count <= 0)
      (List(), rng)
    else {
      val (x, r1) = rng.nextInt
      val (xs, r2) = ints(count - 1)(r1)
      (x :: xs, r2)
    }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  // def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
  //   fs.foldRight((rng: Rand[A]) => {
  //     val (i, r2) = rng.nextInt
  //     (List(), r2)
  //   })((ra, b: Rand[List[A]]) => map2(ra, b)(_ :: _))
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(
      f: (A, B) => C
  ): Rand[C] =
    flatMap(ra) { a =>
      map(rb)(b => f(a, b))
    }
}

//case class State[S,+A](run: S => (A,S)) {
//  def map[B](f: A => B): State[S, B] =
//    s => {
//      val (a, s2) = run(s)
//      (f(a), s2)
//    }
//
//  def map2[B,C](run2: S => (B,S))(f: (A, B) => C): State[S, C] =
//    s => {
//      val (a, s2) = run(s)
//      val (b, s3) = run2(s2)
//      (f(a, b), s3)
//    }
//
//  def flatMap[B](f: A => State[S, B]): State[S, B] =
//    s => {
//      val (a, s2) = run(s)
//      f(a)(s2)
//    }
//}

import State._

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))
  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}

//object State {
//  type Rand[A] = State[RNG, A]
//  def unit[S,A](a: A): State[S, A] = s => (a, s)
//  def sequence[S,A](fs: List[State[S, A]]): State[S, List[A]] =
//    fs.foldRight(unit(List()))((a, acc) => a.map2(acc)(_ :: _))
//}

object State {

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  // The idiomatic solution is expressed via foldRight
  def sequenceViaFoldRight[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))
}
