package fpinscala.parser

import fpinscala.either.{MyLeft, MyRight}
import org.scalatest.{DiagrammedAssertions, FlatSpec}

class ParserReferenceSpec extends FlatSpec with DiagrammedAssertions {

  "firstNonmatchingIndex" should "return the first index where the two strings differed" in {
    assert(ReferenceTypes.firstNonmatchingIndex("hello", "ello", 0) === 0)
    assert(ReferenceTypes.firstNonmatchingIndex("hello", "hollo", 0) === 1)

    assert(ReferenceTypes.firstNonmatchingIndex("hello", "llo", 1) === 0)
    assert(ReferenceTypes.firstNonmatchingIndex("hello", "hello", 0) === -1)
    assert(ReferenceTypes.firstNonmatchingIndex("hello", "ello", 1) === -1)
  }

  "string" should "succeed parse a string" in {
    val MyRight(result) = Reference.run(Reference.string("hello"))("hello")
    assert(result == "hello")
  }

  "`p | q`" should "succeed if `p` succeed or `q` succeed" in {
    val parser = Reference.or(Reference.string("hello"), Reference.string("world"))
    val MyRight(hello) = Reference.run(parser)("hello")
    val MyRight(world) = Reference.run(parser)("world")
    assert(hello === "hello")
    assert(world === "world")
  }


  "`p | q`" should "fail if `p` fail and `q` fail" in {
    val parser = Reference.or(Reference.string("hello"), Reference.string("world"))
    val MyLeft(pr) = Reference.run(parser)("hard")
    val MyLeft(qr) = Reference.run(parser)("work")
    assert(pr.latestLocation.get.offset === 1)
    assert(qr.latestLocation.get.offset === 3)
  }

}
