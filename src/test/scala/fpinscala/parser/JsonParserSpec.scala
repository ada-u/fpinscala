package fpinscala.parser

import fpinscala.either.{MyLeft, MyRight, MyEither}
import org.scalatest.{DiagrammedAssertions, FlatSpec}

class JsonParserSpec extends FlatSpec with DiagrammedAssertions {

  "JsonParser" should "parse JSON object" in {
    val json = """
{
  "key": "value"
}
"""

    val parser = Json.jsonParser(Reference)
    val MyRight(result) = Reference.run(parser)(json)
    assert(result === JsObject(Map("key" -> JsString("value"))))
  }
}
