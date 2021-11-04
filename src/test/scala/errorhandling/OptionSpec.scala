package errorhandling

import org.scalatest._
import errorhandling._
import errorhandling.Option._
import errorhandling.{Option,Some,None}

class OptionSpec extends FlatSpec with Matchers {
  "map" should "map" in {
    val some = Some(1)
    some.map(i => i + i).getOrElse(-1) shouldEqual 2
    val none: Option[Int] = None
    none.map(i => i + i).getOrElse(-1) shouldEqual -1
  }

  "flatMap" should "flatMap" in {
    val some = Some(1)
    some.flatMap(i => Some(i + i)).getOrElse(-1) shouldEqual 2
    val none: Option[Int] = None
    none.flatMap(i => Some(i + i)).getOrElse(-1) shouldEqual -1
  }

  "orElse" should "orElse" in {
    val some = Some(1)
    some.flatMap(i => Some(i + i)).orElse(Some(-1)) shouldEqual Some(2)
    val none: Option[Int] = None
    none.flatMap(i => Some(i + i)).orElse(Some(-1)) shouldEqual Some(-1)
  }

  "filter" should "filter" in {
    val some = Some(1)
    some.filter(a => if (a == 1) true else false) shouldEqual Some(1)
    some.filter(a => if (a == 2) true else false) shouldEqual None
  }

  "sequence" should "sequence" in {
    val someCase = List(Some(1), Some(2), Some(3))
    sequence(someCase) shouldEqual Some(List(1,2,3))

    val noneCase = List(Some(1), None, Some(3))
    sequence(noneCase) shouldEqual None
  }
}
