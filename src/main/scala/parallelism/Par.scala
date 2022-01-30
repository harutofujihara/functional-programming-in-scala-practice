package parallelism

import java.util.concurrent._
import language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def fork[A](a: => Par[A]): Par[A] =
    es =>
      es.submit(new Callable[A] {
        def call = a(es).get
      })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  // def run[A](a: Par[A]): A
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

//  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
//    ps.foldRight(unit(()))((a, b) => map2(b, a))
//  }
  def sequence[A](l: List[Par[A]]): Par[List[A]] =
    l.foldRight[Par[List[A]]](unit(List()))((h, t) => map2(h, t)(_ :: _))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  // def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
  //   val fbs = as.map(a => (a, asyncF(f)(a)))
  //   // fbs.foldRight[Par[List[A]]](unit(List()))(((a, pb), t) => )
  //   fbs.foldRight[Par[List[A]]](unit(List()))((h, t) => {
  //     if (h._2) unit(h._1) :: t
  //     else t
  //   })
  // }

  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] =
      l map (asyncF((a: A) => if (f(a)) List(a) else List()))
    map(sequence(pars))(
      _.flatten
    ) // convenience method on `List` for concatenating a list of lists
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  // def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
  //   es => choices(run(es)(n).get)(es)
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => {
      val ind = run(es)(n).get // Full source files
      run(es)(choices(ind))
    }

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(b => if (b) 0 else 1))(List(t, f))

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es => {
      val k = run(es)(key).get
      run(es)(choices(k))
    }

  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val s = run(es)(pa).get
      run(es)(choices(s))
    }

  def choiceViaChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond)(b => if (b) t else f)

  def choiceNViaChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(n)(i => choices(i))

  /* `chooser` is usually called `flatMap` or `bind`. */
  def flatMap[A, B](p: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val k = run(es)(p).get
      run(es)(choices(k))
    }

  // def join[A](a: Par[Par[A]]): Par[A] =
  //   es => map(a)(p => run(es)(p).get)(es)

  // def flatMapViaJoin[A, B](p: Par[A])(choices: A => Par[B]): Par[B] =
  //   join(map(p)(a => choices(a)))

  // see nonblocking implementation in `Nonblocking.scala`
  def join[A](a: Par[Par[A]]): Par[A] =
    es => run(es)(run(es)(a).get())

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(x => x)

  def flatMapViaJoin[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
    join(map(p)(f))
}
