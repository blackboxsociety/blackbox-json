package com.blackboxsociety.json

trait JsReader[A] {
  def read(value: JsValue): Option[A]
}

object JsReader {

  implicit object booleanJsReader extends JsReader[Boolean] {
    override def read(value: JsValue): Option[Boolean] = value match {
      case JsBoolean(b) => Some(b)
      case _            => None
    }
  }

  implicit object stringJsReader extends JsReader[String] {
    override def read(value: JsValue): Option[String] = value match {
      case JsString(s) => Some(s)
      case _           => None
    }
  }

  implicit object intJsReader extends JsReader[Int] {
    override def read(value: JsValue): Option[Int] = value match {
      case JsInt(i) => Some(i)
      case _        => None
    }
  }

  implicit object floatJsReader extends JsReader[Float] {
    override def read(value: JsValue): Option[Float] = value match {
      case JsFloat(f) => Some(f)
      case _          => None
    }
  }

  implicit def listJsReader[A](implicit c: JsReader[A]) = new JsReader[List[A]] {
    override def read(value: JsValue): Option[List[A]] = value match {
      case JsArray(a) => a.foldLeft[Option[List[A]]](Some(List())) { (m, n) =>
        for (list <- m; current <- c.read(n)) yield list :+ current
      }
      case _ => None
    }
  }

  implicit def MapJsReader[A](implicit c: JsReader[A]) = new JsReader[Map[String, A]] {
    override def read(value: JsValue): Option[Map[String, A]] = value match {
      case JsObject(o) => o.foldLeft[Option[Map[String, A]]](Some(Map())) { (m, n) =>
       for (map <- m; current <- c.read(n._2)) yield map + (n._1 -> current)
      }
      case _ => None
    }
  }

}