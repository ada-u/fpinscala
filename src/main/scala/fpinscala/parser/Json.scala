package fpinscala.parser

sealed trait Json

case object JsNull extends Json
case class JsNumber(get: Double) extends Json
case class JsString(get: String) extends Json
case class JsBoolean(get: Boolean) extends Json
case class JsArray(get: IndexedSeq[Json]) extends Json
case class JsObject(get: Map[String, Json]) extends Json

object Json {

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[Json] = {
    import P._

    def jsKeyValue: Parser[(String, Json)] =
      escapedQuoted ** (":" *> jsValue)

    def jsObject: Parser[Json] =
      surround("{", "}")(jsKeyValue.separate(",").map(kvs => JsObject(kvs.toMap))).scope("JsObject")

    def jsArray: Parser[Json] =
      surround("[", "]")(jsValue.separate(",").map(vs => JsArray(vs.toIndexSeq))).scope("JsArray")

    def jsLiteral: Parser[Json] = {
      "true".as(JsBoolean(get = true))    | // true
      "false".as(JsBoolean(get = false))  | // false
      "null".as(JsNull)                   | // null
      double.map(JsNumber)                | // 27.5
      escapedQuoted.map(JsString)         // "string"
    }

    def jsValue: Parser[Json] = jsLiteral | jsObject | jsArray

    root(whiteSpace *> (jsObject | jsArray))
  }
}