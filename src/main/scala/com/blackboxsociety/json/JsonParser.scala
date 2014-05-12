package com.blackboxsociety.json

import scala.util.parsing.combinator._
import scalaz._

object JsonParser extends RegexParsers {

  override def skipWhitespace = false

  val commaSep = padded(",")

  val textChar = "[^\",\r\n]".r

  val asciiEsc: Parser[String] = "\\" | "\"" | "/"

  val backspaceEsc: Parser[String] = "b" ^^ { _ => "\b" }

  val formFeedEsc: Parser[String] = "f" ^^ { _ => "\f" }

  val newLineEsc: Parser[String] = "n" ^^ { _ => "\n" }

  val carriageReturnEsc: Parser[String] = "r" ^^ { _ => "\r" }

  val tabEsc: Parser[String] = "t" ^^ { _ => "\t" }

  val escapedSeq: Parser[String] =
    "\\" ~> (asciiEsc | backspaceEsc | formFeedEsc | newLineEsc | carriageReturnEsc | tabEsc | "u")

  val booleanTrue: Parser[JsBoolean] = "true" ^^ { _ => JsBoolean(true) }

  val booleanFalse: Parser[JsBoolean] = "false" ^^ { _ => JsBoolean(false) }

  val boolean: Parser[JsBoolean] = booleanTrue | booleanFalse

  val string: Parser[JsString] = "\"" ~> rep(escapedSeq | textChar) <~ "\"" ^^ { n =>
    JsString(n.mkString(""))
  }

  val int: Parser[JsInt] = "[0-9]+".r ^^ { n => JsInt(n.toInt) }

  val float: Parser[JsFloat] = "[0-9]+\\.[0-9]+".r ^^ { n => JsFloat(n.toFloat) }

  val array: Parser[JsArray] = "[" ~> padded(repsep(value, commaSep)) <~ "]" ^^ { n => JsArray(n) }

  val objPairs: Parser[Map[String, JsValue]] = repsep(padded(string) ~ ":" ~ value, commaSep) ^^ { n =>
    n.foldLeft(Map[String, JsValue]()) {
      case (m, k ~ _ ~ v) => m + (k.toString -> v)
    }
  }

  val obj: Parser[JsObject] = "{" ~> padded(objPairs) <~ "}" ^^ { n => JsObject(n) }

  val nullP: Parser[JsNull.type] = "null" ^^ { _ => JsNull }

  private def padded[A](parser: Parser[A]): Parser[A] = opt(whiteSpace) ~> parser <~ opt(whiteSpace)

  def value: Parser[JsValue] = padded(boolean | string | float | int | array | obj | nullP)

  def parse(input: String): Validation[String, JsValue] = parseAll(value, input) match {
    case Success(json, _)    => scalaz.Success(json)
    case NoSuccess(error, _) => scalaz.Failure(error)
  }

}