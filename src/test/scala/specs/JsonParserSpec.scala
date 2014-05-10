package specs

import org.specs2.mutable._
import com.blackboxsociety.json._
import scalaz._

class JsonParserSpec extends Specification {

  "A valid boolean false value" should {
    "correctly parse into a JsBoolean(false)" in {
      JsonParser.parse("false") must beEqualTo(Success(JsBoolean(false)))
    }
  }

  "A valid boolean false padded with space prior to it" should {
    "correctly parse into a JsBoolean(false)" in {
      JsonParser.parse("   false") must beEqualTo(Success(JsBoolean(false)))
    }
  }

  "A valid boolean false padded with space after it" should {
    "correctly parse into a JsBoolean(false)" in {
      JsonParser.parse("false   ") must beEqualTo(Success(JsBoolean(false)))
    }
  }

  "A valid boolean false padded with space before and after it" should {
    "correctly parse into a JsBoolean(false)" in {
      JsonParser.parse("   false   ") must beEqualTo(Success(JsBoolean(false)))
    }
  }

  "A valid boolean true value" should {
    "correctly parse into a JsBoolean(true)" in {
      JsonParser.parse("true") must beEqualTo(Success(JsBoolean(true)))
    }
  }

  "A valid boolean true padded with space prior to it" should {
    "correctly parse into a JsBoolean(true)" in {
      JsonParser.parse("   true") must beEqualTo(Success(JsBoolean(true)))
    }
  }

  "A valid boolean true padded with space after it" should {
    "correctly parse into a JsBoolean(true)" in {
      JsonParser.parse("true   ") must beEqualTo(Success(JsBoolean(true)))
    }
  }

  "A valid boolean true padded with space before and after it" should {
    "correctly parse into a JsBoolean(true)" in {
      JsonParser.parse("   true   ") must beEqualTo(Success(JsBoolean(true)))
    }
  }

  "An empty string literal" should {
    "correctly parse into a JsString(\"\")" in {
      JsonParser.parse("\"\"") must beEqualTo(Success(JsString("")))
    }
  }

  "An empty string literal padded with space prior to it" should {
    "correctly parse into a JsString(\"\")" in {
      JsonParser.parse("   \"\"") must beEqualTo(Success(JsString("")))
    }
  }

  "An empty string literal padded with space after it" should {
    "correctly parse into a JsString(\"\")" in {
      JsonParser.parse("\"\"   ") must beEqualTo(Success(JsString("")))
    }
  }

  "An empty string literal padded with space before and after it" should {
    "correctly parse into a JsString(\"\")" in {
      JsonParser.parse("   \"\"   ") must beEqualTo(Success(JsString("")))
    }
  }

  "An Hello World! string literal" should {
    "correctly parse into a JsString(\"Hello World!\")" in {
      JsonParser.parse("\"Hello World!\"") must beEqualTo(Success(JsString("Hello World!")))
    }
  }

  "An Hello World! string literal padded with space prior to it" should {
    "correctly parse into a JsString(\"Hello World!\")" in {
      JsonParser.parse("   \"Hello World!\"") must beEqualTo(Success(JsString("Hello World!")))
    }
  }

  "An Hello World! string literal padded with space after it" should {
    "correctly parse into a JsString(\"Hello World!\")" in {
      JsonParser.parse("\"Hello World!\"   ") must beEqualTo(Success(JsString("Hello World!")))
    }
  }

  "An Hello World! string literal padded with space before and after it" should {
    "correctly parse into a JsString(\"Hello World!\")" in {
      JsonParser.parse("   \"Hello World!\"   ") must beEqualTo(Success(JsString("Hello World!")))
    }
  }

  "An She said \"wat\"! string literal" should {
    "correctly parse into a JsString(\"She said \"wat\"!\")" in {
      JsonParser.parse("\"She said \\\"wat\\\"!\"") must beEqualTo(Success(JsString("She said \"wat\"!")))
    }
  }

  "An She said \"wat\"! string literal padded with space prior to it" should {
    "correctly parse into a JsString(\"She said \"wat\"!\")" in {
      JsonParser.parse("   \"She said \\\"wat\\\"!\"") must beEqualTo(Success(JsString("She said \"wat\"!")))
    }
  }

  "An She said \"wat\"! string literal padded with space after it" should {
    "correctly parse into a JsString(\"She said \"wat\"!\")" in {
      JsonParser.parse("\"She said \\\"wat\\\"!\"   ") must beEqualTo(Success(JsString("She said \"wat\"!")))
    }
  }

  "An She said \"wat\"! string literal padded with space before and after it" should {
    "correctly parse into a JsString(\"She said \"wat\"!\")" in {
      JsonParser.parse("   \"She said \\\"wat\\\"!\"   ") must beEqualTo(Success(JsString("She said \"wat\"!")))
    }
  }

  "A string literal containing \\\\" should {
    "correctly translate the \\\\" in {
      JsonParser.parse("\"\\\\\"") must beEqualTo(Success(JsString("\\")))
    }
  }

  "A string literal containing \\\"" should {
    "correctly translate the \\\"" in {
      JsonParser.parse("\"\\\"\"") must beEqualTo(Success(JsString("\"")))
    }
  }

  "A string literal containing \\/" should {
    "correctly translate the \\/" in {
      JsonParser.parse("\"\\/\"") must beEqualTo(Success(JsString("/")))
    }
  }

  "A string literal containing \\b" should {
    "correctly translate the \\b" in {
      JsonParser.parse("\"\\b\"") must beEqualTo(Success(JsString("\b")))
    }
  }

  "A string literal containing \\f" should {
    "correctly translate the \\f" in {
      JsonParser.parse("\"\\f\"") must beEqualTo(Success(JsString("\f")))
    }
  }

  "A string literal containing \\n" should {
    "correctly translate the \\n" in {
      JsonParser.parse("\"\\n\"") must beEqualTo(Success(JsString("\n")))
    }
  }

  "A string literal containing \\r" should {
    "correctly translate the \\r" in {
      JsonParser.parse("\"\\r\"") must beEqualTo(Success(JsString("\r")))
    }
  }

  "A string literal containing \\t" should {
    "correctly translate the \\t" in {
      JsonParser.parse("\"\\t\"") must beEqualTo(Success(JsString("\t")))
    }
  }

  "An int literal of 42" should {
    "parse correctly JsInt(42)" in {
      JsonParser.parse("42") must beEqualTo(Success(JsInt(42)))
    }
  }

  "An int literal of 42 with whitespace prior to it" should {
    "parse correctly JsInt(42)" in {
      JsonParser.parse("   42") must beEqualTo(Success(JsInt(42)))
    }
  }

  "An int literal of 42 with whitespace after it" should {
    "parse correctly JsInt(42)" in {
      JsonParser.parse("42   ") must beEqualTo(Success(JsInt(42)))
    }
  }

  "An int literal of 42 with whitespace before and after it" should {
    "parse correctly JsInt(42)" in {
      JsonParser.parse("   42   ") must beEqualTo(Success(JsInt(42)))
    }
  }

  "A float literal of 4.2" should {
    "parse correctly JsFloat(4.2)" in {
      JsonParser.parse("4.2") must beEqualTo(Success(JsFloat(4.2f)))
    }
  }

  "A float literal of 4.2 with whitespace prior to it" should {
    "parse correctly JsFloat(4.2)" in {
      JsonParser.parse("   4.2") must beEqualTo(Success(JsFloat(4.2f)))
    }
  }

  "A float literal of 4.2 with whitespace after it" should {
    "parse correctly JsFloat(4.2)" in {
      JsonParser.parse("4.2   ") must beEqualTo(Success(JsFloat(4.2f)))
    }
  }

  "A float literal of 4.2 with whitespace before and after it" should {
    "parse correctly JsFloat(4.2)" in {
      JsonParser.parse("   4.2   ") must beEqualTo(Success(JsFloat(4.2f)))
    }
  }

  "An empty array litteral of []" should {
    "parse correctly as JsArray(Seq())" in {
      JsonParser.parse("[]") must beEqualTo(Success(JsArray(List())))
    }
  }

  "An empty array litteral of [] with whitespace prior to it" should {
    "parse correctly as JsArray(Seq())" in {
      JsonParser.parse("   []") must beEqualTo(Success(JsArray(List())))
    }
  }

  "An empty array litteral of [] with whitespace after it" should {
    "parse correctly as JsArray(Seq())" in {
      JsonParser.parse("[]   ") must beEqualTo(Success(JsArray(List())))
    }
  }

  "An empty array litteral of [] with whitespace before and after it" should {
    "parse correctly as JsArray(Seq())" in {
      JsonParser.parse("    []   ") must beEqualTo(Success(JsArray(List())))
    }
  }

  "An array litteral of [42, \\\"lol\\\", {}, true, [6]]" should {
    "parse correctly" in {
      val r = Success(JsArray(List(JsInt(42), JsString("lol"), JsObject(Map()), JsBoolean(true), JsArray(List(JsInt(6))))))
      JsonParser.parse("[42, \"lol\", {}, true, [6]]") must beEqualTo(r)
    }
  }

  "An empty object literal {}" should {
    "correctly parse to {}" in {
      JsonParser.parse("{}") must beEqualTo(Success(JsObject(Map())))
    }
  }

  "An empty object literal {} with whitespace prior to it" should {
    "correctly parse to {}" in {
      JsonParser.parse("   {}") must beEqualTo(Success(JsObject(Map())))
    }
  }


  "An empty object literal {} with whitespace after it" should {
    "correctly parse to {}" in {
      JsonParser.parse("{}   ") must beEqualTo(Success(JsObject(Map())))
    }
  }

  "An empty object literal {} with whitespace before and after it" should {
    "correctly parse to {}" in {
      JsonParser.parse("   {}   ") must beEqualTo(Success(JsObject(Map())))
    }
  }

  """An object literal of {"a": 42, "b": "v", "c": [1, 2], "d": true}""" should {
    "correctly parse" in {
      val in  = "{\"a\": 42, \"b\": \"v\", \"c\": [1, 2], \"d\": true}"
      val out = Success(JsObject(Map(
        "a" -> JsInt(42),
        "b" -> JsString("v"),
        "c" -> JsArray(List(JsInt(1), JsInt(2))),
        "d" -> JsBoolean(true)
      )))
      JsonParser.parse(in) must beEqualTo(out)
    }
  }

  """An object literal of {  "a"  : 42, "b"  : "v"  , "c"  : [1, 2] , "d"  : true }""" should {
    "correctly parse" in {
      val in  = "{\"a\": 42, \"b\": \"v\", \"c\": [1, 2], \"d\": true}"
      val out = Success(JsObject(Map(
        "a" -> JsInt(42),
        "b" -> JsString("v"),
        "c" -> JsArray(List(JsInt(1), JsInt(2))),
        "d" -> JsBoolean(true)
      )))
      JsonParser.parse(in) must beEqualTo(out)
    }
  }

}
