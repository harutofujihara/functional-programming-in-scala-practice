package testing

import state._
import laziness.Stream
import parallelism._
import parallelism.Par.Par
import java.util.concurrent.{Executors, ExecutorService}
import Gen._
import Prop._

// trait Prop { def &&(p: Prop): Prop }

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
  // type Result = Option[(FailedCase, SuccessCount)]
  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase, successes: SuccessCount)
      extends Result {
    def isFalsified = true
  }
  case object Proved extends Result {
    def isFalsified = false
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (n, rng) =>
    randomStream(as)(rng)
      .zip(Stream.from(0))
      .take(n)
      .map { case (a, i) =>
        try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
      }
      .find(_.isFalsified)
      .getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)
  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n.min(max)) + 1).map(i => forAll(g(i))(f))
      val prop: Prop = props
        .map(p => Prop { (max, _, rng) => p.run(max, casesPerSize, rng) })
        .toList
        .reduce(_ && _)
      prop.run(max, n, rng)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  // String interpolation syntax. A string starting with `s"` can refer to
  // a Scala value `v` as `$v` or `${v}` in the string.
  // This will be expanded to `v.toString` by the Scala compiler.
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  // def check(p: => Boolean): Prop = {
  //   lazy val result = p
  //   forAll(Gen.unit())(_ => result)
  // }
  val ES: ExecutorService = Executors.newCachedThreadPool
  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Passed else Falsified("()", 0)
  }

  // val p2 = check {
  //   val p = Par.map(Par.unit(1))(_ + 1)
  //   val p2 = Par.unit(2)
  //   p(ES).get == p2(ES).get()
  // }

  val p2 = checkPar {
    equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )
  }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p, p2)(_ == _)
  val p3 = check {
    equal(Par.map(Par.unit(1))(_ + 1), Par.unit(2))(ES).get
  }

  val S = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25
  ) // a -> b は(a,b)の糖衣構文

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S.map2(g)((_, _))) { case (s, a) => f(a)(s).get }

  def checkPar(p: Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

  def forAllPar2[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case (s, a) => f(a)(s).get }

  def forAllPar3[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case s ** a => f(a)(s).get }
}

// trait Prop { def check: Either[(FailedCase, SuccessCount), SuccessCount] }
// case class Prop(run: (TestCases, RNG) => Result) {
//   def &&(p: Prop): Prop = Prop { (n, rng) =>
//     this.run(n, rng) match {
//       case falsified: Falsified => falsified
//       case Passed =>
//         p.run(n, rng) match {
//           case falsified2: Falsified => falsified2
//           case Passed                => Passed
//         }
//     }
//   }

//   def ||(p: Prop): Prop = Prop { (n, rng) =>
//     this.run(n, rng) match {
//       case Passed => Passed
//       case falsified: Falsified =>
//         p.run(n, rng) match {
//           case Passed                => Passed
//           case falsified2: Falsified => falsified2
//         }
//     }
//   }
// }
case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop) = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case Passed | Proved => p.run(max, n, rng)
      case x               => x
    }
  }

  def ||(p: Prop) = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      // In case of failure, run the other prop.
      case Falsified(msg, _) => p.tag(msg).run(max, n, rng)
      case x                 => x
    }
  }

  /* This is rather simplistic - in the event of failure, we simply prepend
   * the given message on a newline in front of the existing message.
   */
  def tag(msg: String) = Prop { (max, n, rng) =>
    run(max, n, rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x               => x
    }
  }
}

case class Gen[A](sample: State[RNG, A]) {
  // def choose(start: Int, stopExclusive: Int): Gen[Int] = {
  //   //
  // }
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))
  // def flatMap[B](f: A => Gen[B]): Gen[B] =
  //   Gen(sample.map(f))
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(i => Gen.listOfN(i, this))

  def unsized: SGen[A] = SGen(i => this)

  def **[B](g: Gen[B]): Gen[(A, B)] =
    (this map2 g)((_, _))

}

object Gen {
  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  // def boolean: Gen[Boolean] = Gen(State.unit(true))

  // def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
  //   //
  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(listOfN(_, g))

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => listOfN(n.max(1), g))

  // def union[A](g1:Gen[A], g2:Gen[A]): Gen[A] =
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  // def weighted[A](g1: (Gen[A], Double), g2:(Gen[A],Double)): Gen[A] =
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    /* The probability we should pull from `g1`. */
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(
      State(RNG.double).flatMap(d =>
        if (d < g1Threshold) g1._1.sample else g2._1.sample
      )
    )
  }

  val smallInt = Gen.choose(-10, 10)
  val maxProp = forAll(listOf(smallInt)) { l =>
    val max = l.max
    !l.exists(_ > max) // No value greater than `max` should exist in `l`
  }

  // val sortProp = forAll(listOf(smallInt))(l =>
  //   l.sliding(2).forall(pair => pair(0) <= pair(1))
  // )

  val sortedProp = forAll(listOf(smallInt)) { ns =>
    val nss = ns.sorted
    // We specify that every sorted list is either empty, has one element,
    // or has no two consecutive elements `(a,b)` such that `a` is greater than `b`.
    (nss.isEmpty || nss.tail.isEmpty || !nss.zip(nss.tail).exists {
      case (a, b) => a > b
    }) && !ns.exists(!nss.contains(_)) && !nss.exists(!ns.contains(_))
  }

  object ** {
    def unapply[A, B](p: (A, B)) = Some(p)
  }

  def run(
      p: Prop,
      maxSize: Int = 100,
      testCases: Int = 100,
      rng: RNG = RNG.SimpleRNG(System.currentTimeMillis)
  ): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:¥n $msg")
      case Passed => println(s"+ oK, passed $testCases tests.")
      case Proved => println(s"+ OK, proved property.")
    }
}

// case class SGen[+A](forSize: Int => Gen[A]) {
//   def choose(start: Int, stopExclusive: Int): Gen[Int] =
//     Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

//   def flatMap[B](f: A => SGen[B]): SGen[B] =
//     SGen(i => this.forSize(i).flatMap(a => f(a).forSize(i)))

// }

case class SGen[+A](g: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = g(n)

  def map[B](f: A => B): SGen[B] =
    SGen { g(_) map f }

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val g2: Int => Gen[B] = n => {
      g(n) flatMap { f(_).g(n) }
    }
    SGen(g2)
  }

  def **[B](s2: SGen[B]): SGen[(A, B)] =
    SGen(n => apply(n) ** s2(n))
}
