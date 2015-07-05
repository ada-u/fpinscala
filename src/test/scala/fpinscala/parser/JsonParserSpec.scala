package fpinscala.parser

import fpinscala.collection.list.MyList
import fpinscala.either.{MyEither, MyRight}
import org.scalatest.{DiagrammedAssertions, FlatSpec}

import scala.language.higherKinds

class JsonParserSpec extends FlatSpec with DiagrammedAssertions {

  def printResult[E](e: MyEither[E, ((String, MyList[String]), String)]) =
    e.fold(println, println)

  val parser = Json.jsonParser(Reference)

    "JsLiteral" should "parser a `true` literal" in {
      val json = "{ \"key\": true }"
      val MyRight(result) = Reference.run(parser)(json)
      assert(result === JsObject(Map("key" -> JsBoolean(get = true))))
    }
/*
  "JsonParser" should "parse JSON object" in {
    val json = """
{
  "key": "value"
}
               """

    val parser = Json.jsonParser(Reference)
    val MyRight(result) = Reference.run(parser)(json)
    assert(result === JsObject(Map("key" -> JsString("value"))))
  }*/
}
