package fpinscala.parser

import fpinscala.collection.list.MyList
import fpinscala.option.MyOption

case class ParseError(stack: MyList[(Location, String)]) {

  def push(location: Location, message: String): ParseError =
    copy(stack = (location, message) :: stack)

  def label[A](s: String): ParseError =
    ParseError(latestLocation.map((_, s)).toList)

  def latest: MyOption[(Location, String)] =
    stack.lastOption

  def latestLocation: MyOption[Location] =
    latest.map(_._1)
}