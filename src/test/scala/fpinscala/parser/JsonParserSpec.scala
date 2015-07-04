package fpinscala.parser

import fpinscala.collection.list.MyList
import fpinscala.either.{MyEither, MyRight}
import fpinscala.parser.ReferenceTypes.Parser
import fpinscala.testing.{Gen, Prop, SGen}
import org.scalatest.{DiagrammedAssertions, FlatSpec}

import scala.language.higherKinds

class JsonParserSpec extends FlatSpec with DiagrammedAssertions {

  def printResult[E](e: MyEither[E, ((String, MyList[String]), String)]) =
    e.fold(println, println)

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
