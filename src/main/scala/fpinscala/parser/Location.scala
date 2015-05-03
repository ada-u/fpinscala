package fpinscala.parser

import fpinscala.collection.list.MyList
import fpinscala.parser.ReferenceTypes.Failure

case class Location(input: String, offset: Int = 0) {

  lazy val line: Int = input.slice(0, offset + 1).count(_ == '\n') + 1

  lazy val column: Int = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }

  def advanceBy(n: Int): Location =
    copy(offset = offset + n)

  def toError(message: String): ParseError =
    ParseError(MyList((this, message)))

}