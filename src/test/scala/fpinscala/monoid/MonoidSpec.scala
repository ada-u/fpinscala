package fpinscala.monoid

import fpinscala.collection.list.MyList
import fpinscala.testing.{Prop, Gen}
import org.scalatest.{DiagrammedAssertions, FlatSpec}

class MonoidSpec extends FlatSpec with DiagrammedAssertions {

  "A stringMonoid" should "satisfy Monoid Laws" in {
    Prop.run(Monoid.monoidLaws(Monoid.stringMonoid, Gen.string(100)))
  }

  "An intAddition monoid" should "satisfy Monoid Laws" in {
    Prop.run(Monoid.monoidLaws(Monoid.intAddition, Gen.choose(0, 100)))
  }

  "An booleanOr monoid" should "satisfy Monoid Laws" in {
    Prop.run(Monoid.monoidLaws(Monoid.booleanOr, Gen.boolean))
  }

  "An booleanAnd monoid" should "satisfy Monoid Laws" in {
    Prop.run(Monoid.monoidLaws(Monoid.booleanAnd, Gen.boolean))
  }

  "A listMonoid" should "satisfy Monoid Laws" in {
    Prop.run(Monoid.monoidLaws[MyList[Int]](Monoid.listMonoid[Int], Gen.listOf(Gen.choose(-100, 100)).forSize(10)))
  }

  "ToMonoidOp" should "inject a MonoidOp" in {
    import Monoid.intAddition
    import ToMonoidOps._
    assert((1 |+| 1) === 2)
  }

}