package specs

import org.specs2.mutable._
import com.blackboxsociety.json._

class JsWriterSpec extends Specification {

  "The boolean json writer" should {
    "convert a boolean true to a JsBoolean(true)" in {
      JsValue.from(true) must beEqualTo(JsBoolean(true))
    }
    "convert a boolean false to a JsBoolean(true)" in {
      JsValue.from(false) must beEqualTo(JsBoolean(false))
    }
  }

  "The string json writer" should {
    "convert a string \"test\" to a JsString(\"test\")" in {
      JsValue.from("test") must beEqualTo(JsString("test"))
    }
  }

  "The int json writer" should {
    "convert a int from 42 to JsInt(42)" in {
      JsValue.from(42) must beEqualTo(JsInt(42))
    }
  }

  "The float json writer" should {
    "convert a float from 4.2 to JsFloat(4.2)" in {
      JsValue.from(4.2f) must beEqualTo(JsFloat(4.2f))
    }
  }

  "The list json writer" should {
    "convert a List(1, 2, 3) to a JsArray(List(JsInt(1), JsInt(2), JsInt(3)))" in {
      JsValue.from(List(1, 2, 3)) must beEqualTo(JsArray(List(JsInt(1), JsInt(2), JsInt(3))))
    }
  }

  "The map json writer" should {
    "convert a Map(\"a\" -> 1, \"b\" -> 2) to a JsObject(Map(Map(\"a\" -> JsInt(1), \"b\" -> JsInt(2))))" in {
      JsValue.from(Map("a" -> 1, "b" -> 2)) must beEqualTo(JsObject(Map("a" -> JsInt(1), "b" -> JsInt(2))))
    }
  }

}
