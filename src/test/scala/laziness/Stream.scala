package laziness

import org.scalatest._
import laziness.Stream._
import laziness.{Stream,Cons,Empty}

class StreamSpec extends FlatSpec with Matchers {
  "take" should "take first n elements" in {
    val s = Stream.apply(1,2,3,4,5)

    s.take(2).toListRecursive shouldEqual List(1,2)
  }

  "drop" should "drop first n elements" in {
    val s = Stream.apply(1,2,3,4,5)

    s.drop(3).toListRecursive shouldEqual List(4, 5)
  }

  "takeWhile" should "takeWhile" in {
    val s = Stream.apply(1,2,3,4,5)

    s.takeWhile(n => n < 3).toListRecursive shouldEqual List(1, 2)
  }

  "forAll" should "forAll" in {
    val s = Stream.apply(1,2,3,4,5)

    s.forAll(n => n < 3) shouldEqual false
  }

  "takeWhileViaFoldRight" should "takeWhileViaFoldRight" in {
    val s = Stream.apply(1,2,3,4,5)

    s.takeWhileViaFoldRight(n => n < 3).toListRecursive shouldEqual List(1, 2)
  }

  "headOptionViaFoldRight" should "headOptionViaFoldRight" in {
    val s = Stream.apply(1,2,3,4,5)

    s.headOptionViaFoldRight shouldEqual Some(1)
  }

  "map" should "map function" in {
    val s = Stream.apply(1,2,3,4,5)

    s.map(i => i + i).toListRecursive shouldEqual List(2,4,6,8,10)
  }

  "filter" should "filter" in {
    val s = Stream.apply(1,2,3,4,5)

    s.filter(i => i % 2 == 0).toListRecursive shouldEqual List(2,4)
  }

  "append" should "append" in {
    val s1 = Stream.apply(1,2,3)
    val s2 = Stream.apply(4,5,6)

    s1.append(s2).toListRecursive shouldEqual List(1,2,3,4,5,6)
  }

  "flatMap" should "flatMap" in {
    val s = Stream.apply(1,2,3)

    s.flatMap(i => Stream.apply(i, i)).toListRecursive shouldEqual List(1,1,2,2,3,3)
  }

  "from" should "from" in {
    val s = Stream.from(30)

    s.take(3).toListRecursive shouldEqual List(30,31,32)
  }

  "fibs" should "fibs" in {
    val s = Stream.fibs

    s.take(6).toListRecursive shouldEqual List(0,1,1,2,3,5)
  }

  "unfold" should "unfold" in {
    val s = Stream.unfold(1)(i => Some((i.toString, i + i)))

    s.take(3).toListRecursive shouldEqual List("1", "2", "4")
  }

  "fibsViaUnfold" should "fibsViaUnfold" in {
    val s = Stream.fibsViaUnfold

    s.take(6).toListRecursive shouldEqual List(0,1,1,2,3,5)
  }

  "fromViaUnfold" should "fromViaUnfold" in {
    val s = Stream.fromViaUnfold(30)

    s.take(3).toListRecursive shouldEqual List(30,31,32)
  }

  "constantViaUnfold" should "constantViaUnfold" in {
    val s = Stream.constantViaUnfold(30)

    s.take(3).toListRecursive shouldEqual List(30,30,30)
  }

  "mapViaUnfold" should "mapViaUnfold" in {
    val s = Stream.apply(1,2,3,4,5)

    s.mapViaUnfold(i => i + i).toListRecursive shouldEqual List(2,4,6,8,10)
  }

  "takeViaUnfold" should "take first n elements" in {
    val s = Stream.apply(1,2,3,4,5)

    s.takeViaUnfold(2).toListRecursive shouldEqual List(1,2)
  }

  "takeWhileViaUnfold" should "takeWhile" in {
    val s = Stream.apply(1,2,3,4,5)

    s.takeWhileViaUnfold(n => n < 3).toListRecursive shouldEqual List(1, 2)
  }

  "zipWith" should "zip" in {
    Stream.apply(1,2,3).zipWith(Stream.apply(4,5,6,7))(_ + _).toListRecursive shouldEqual List(5,7,9)
  }

  "zipAll" should "zipAll" in {
    val s1 = Stream.apply(1,2)
    val s2 = Stream.apply(4,5,6)

    s1.zipAll(s2).toListRecursive shouldEqual List((Some(1), Some(4)), (Some(2), Some(5)), (None, Some(6)))
  }

  "startsWith" should "startsWith" in {
    val s1 = Stream.apply(1,2,3,4)
    val s2 = Stream.apply(1,2)

    s2.startsWith(s1) shouldEqual false
  }

  "scanRight" should "scanRight" in {
    Stream.apply(1,2,3).scanRight(0)(_ + _).toListRecursive shouldEqual List(6,5,3,0)
  }

//  "tails" should "tails" in {
//    val s = Stream.apply(1,2,3)
//
//    s.tails.map(_.toListRecursive).toListRecursive shouldEqual List(List(1,2,3), List(2,3), List(3))
//  }
}
