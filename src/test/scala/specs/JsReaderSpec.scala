package specs

import org.specs2.mutable._
import com.blackboxsociety.json._

class JsReaderSpec extends Specification {

  "The boolean json reader" should {
    "convert a JsBoolean(true) to Boolean Some(true)" in {
      JsBoolean(true).as[Boolean] must beEqualTo(Some(true))
    }
    "convert a JsBoolean(false) to Some(false)" in {
      JsBoolean(false).as[Boolean] must beEqualTo(Some(false))
    }
    "convert a JsInt(42) to None" in {
      JsInt(42).as[Boolean] must beEmpty
    }
  }

  "The string json reader" should {
    "convert a JsString(\"test\") to Some(\"test\")" in {
      JsString("test").as[String] must beEqualTo(Some("test"))
    }
    "convert a JsBoolean(true) to None" in {
      JsBoolean(true).as[String] must beEqualTo(None)
    }
  }

  "The int json reader" should {
    "convert a JsInt(42) to Some(42)" in {
      JsInt(42).as[Int] must beEqualTo(Some(42))
    }
    "convert a JsFloat(4.2) to None" in {
      JsFloat(4.2f).as[Int] must beEmpty
    }
  }

  "The float json reader" should {
    "convert a JsFloat(4.2) to Some(4.2)" in {
      JsFloat(4.2f).as[Float] must beEqualTo(Some(4.2f))
    }
    "convert a JsInt(42) to None" in {
      JsInt(42).as[Float] must beEmpty
    }
  }

  "The list json reader parameterized with Int" should {
    "convert a JsArray(List(JsInt(1), JsInt(2), JsInt(3), JsInt(4))) to Some(List(1, 2, 3, 4))" in {
      JsArray(List(JsInt(1), JsInt(2), JsInt(3), JsInt(4))).as[List[Int]] must beEqualTo(Some(List(1, 2, 3, 4)))
    }
    "convert a JsArray(List(JsBoolean(true))) to None" in {
      JsArray(List(JsBoolean(true))).as[List[Int]] must beEmpty
    }
  }

  "The map json reader parameterized with Int" should {
    "convert a JsObject(Map(\"a\" -> JsInt(1), \"b\" -> JsInt(2))) to Some(Map(\"a\" -> 1, \"b\" -> 2))" in {
      JsObject(Map("a" -> JsInt(1), "b" -> JsInt(2))).as[Map[String, Int]] must beEqualTo(Some(Map("a" -> 1, "b" -> 2)))
    }
    "convert a JsObject(Map(\"a\" -> JsBoolean(false))) to None" in {
      JsObject(Map("a" -> JsBoolean(false))).as[Map[String, Int]] must beEmpty
    }
  }

}
