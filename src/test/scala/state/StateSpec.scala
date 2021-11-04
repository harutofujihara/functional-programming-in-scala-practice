package state

import org.scalatest._
import state._

class StateSpec extends FlatSpec with Matchers {
  "sequence" should "sequence" in {
    val some = Some(1)
    some.map(i => i + i).getOrElse(-1) shouldEqual 2
    val none: Option[Int] = None
    none.map(i => i + i).getOrElse(-1) shouldEqual -1
  }
}
