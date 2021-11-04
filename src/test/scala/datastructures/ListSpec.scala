package datastructures

import org.scalatest._
import datastructures.{List,Nil,Cons}
import datastructures.List._
import datastructures.List.{length => lengthF}

class ListSpec extends FlatSpec with Matchers {
  "The Hello object" should "say hello" in {
//    Hello.greeting shouldEqual "hello"
    def f(n: Int): Boolean = n == 1
    dropWhile(List(1,2), f) shouldEqual List(2)
  }

  "List init function" should "return list excpet end element" in {
    init(List(1,2,3,4)) shouldEqual List(1,2,3)
  }

  "List length function using foldRight" should "return list length" in {
    lengthF(List(1,2,3,4,5)) shouldEqual 5
  }

  "List reverse" should "reverse list" in {
    reverse(List(1,2,3,4,5)) shouldEqual List(5,4,3,2,1)
  }

  "List append" should "merge two lists" in {
    append2(List(1,2), List(3,4)) shouldEqual List(1,2,3,4)
  }

  "List flat" should "flat list" in {
    flat(List(List(1,2), List(3, 4), List(5,6), List(7,8))) shouldEqual List(1,2,3,4,5,6,7,8)
  }

  "List add one" should "add one each element" in {
    addOne(List(1,2,3)) shouldEqual List(2,3,4)
  }

  "double to string" should "" in {
    doubleToString(List[Double](1.toDouble,2.toDouble,3.toDouble)) shouldEqual List[String]("1.0", "2.0", "3.0")
  }

  "map" should "map function to each element" in {
    map(List(1.toDouble, 2.toDouble))((a: Double) => a.toString) shouldEqual List("1.0", "2.0")
  }

  "filter" should "remove elements not satisfying condition" in {
    filter(List(1,2,3,4))(a => a % 2 == 0) shouldEqual List(2,4)
  }

  "flatMap" should "map and concat" in {
    flatMap(List(1, 2, 3))(a => List(a,a)) shouldEqual List(1,1,2,2,3,3)
  }

  "filterViaFlatMap" should "remove elements not satisfying condition" in {
    filterViaFlatMap(List(1,2,3,4))(a => a % 2 == 0) shouldEqual List(2,4)
  }

  "plusEachElement" should "" in {
    plusEachElement(List(1,2,3), List(4,5,6)) shouldEqual List(5,7,9)
  }

  "zipWith" should "zip" in {
    zipWith(List(1,2,3), List(4,5,6))((a1, a2) => a1 + a2) shouldEqual List(5,7,9)
  }

  "hasSubsequence" should "detect subsequence" in {
    hasSubsequence(List(1,2,3,4), List(1,2)) shouldEqual true
    hasSubsequence(List(1,2,3,4), List(2,3)) shouldEqual true
    hasSubsequence(List(1,2,3,4), List(4)) shouldEqual true
    hasSubsequence(List(1,2,3,4), List(5)) shouldEqual false
//    hasSubsequence(List(1,2,3,4), Nil) shouldEqual true // error
  }
}
