package fpinscala.testing

import fpinscala.collection.list.MyList
import fpinscala.collection.stream.MyStream
import fpinscala.state.RNG
import org.scalatest.{DiagrammedAssertions, FlatSpec}

class GenSpec extends FlatSpec with DiagrammedAssertions {

  val isEven = (i: Int) => i%2 == 0
  val g: Gen[Int => Boolean] = Gen.intBoolean(Gen.boolean)
  val gg: Gen[Int => Boolean] = Gen.fn(Cogen[Int]((rng: RNG, i: Int) =>
    RNG.SimpleRNG(rng.int._1 + i)
  ))(Gen.boolean)

  val prop = Prop.forAll(Gen.listOf[Int](Gen.smallInt)) {
    (ns: MyList[Int]) =>
      ns.toList.takeWhile(isEven).forall(isEven)
  }

  val propA = Prop.forAll(g) {
    (f: Int => Boolean) =>
      (1 to 1000).toList.takeWhile(f).forall(f)
  }

  val propB = Prop.forAll(gg) {
    (f: Int => Boolean) =>
      (1 to 1000).toList.takeWhile(f).forall(f)
  }

  val rng = RNG.SimpleRNG(0)

  val law = Prop.forAll(Gen.smallInt) { a =>
    val left  = Gen.unit(a).map(identity)
    val right = Gen.unit(a)
    Gen.unit(a).map(identity) == Gen.unit(a)
  }

  "Gen" should "satisfy identity Law" in {
    Prop.run(law)
  }


  "A stringMonoid" should "satisfy Monoid Laws" in {
    assert(MyList(1,2,3,4).takeWhile(isEven) === MyList.empty[Int])
    assert(MyList.empty[Int].takeWhile(isEven) === MyList.empty[Int])
    assert(MyList.empty[Int].takeWhile(_ == 1) === MyList.empty[Int])
    assert(MyList(1,2,3,4).takeWhile(_ == 1) === MyList(1))
    assert(MyList(1,2,3,4).takeWhile(_ == 3) === MyList())
    assert(MyList(1,2,3,4).takeWhile(_ < 3) === MyList(1,2))
    assert(MyList(1,2,3,4).takeWhile(_ < 0) === MyList())
    assert(MyList(1,2,3,4).takeWhile(_ > 0) === MyList(1,2,3,4))

    Prop.run(prop)
    Prop.run(propA)
    Prop.run(propB)


  }

}