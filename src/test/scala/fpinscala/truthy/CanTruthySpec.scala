package fpinscala.truthy

import org.scalatest.{ DiagrammedAssertions, FlatSpec }

class CanTruthySpec extends FlatSpec with DiagrammedAssertions {

  "toCanTruthyOps" should "inject ops to truthy" in {
    import CanTruthy._
    import ToCanTruthyOps._

    assert(0.truthy === false)
    assert(1.truthy === true)
  }

}
