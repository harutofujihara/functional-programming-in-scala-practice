package errorhandling

import org.scalatest._
import errorhandling._
import errorhandling.Either._
import errorhandling.{Either,Right,Left}

class EitherSpec extends FlatSpec with Matchers {
  "sequence" should "sequence" in {
    val someCase = List(Right(1), Right(2), Right(3))
    sequence(someCase) shouldEqual Right(List(1,2,3))

    val noneCase = List(Right(1), Left("error"), Right(3))
    sequence(noneCase) shouldEqual Left("error")
  }
}
