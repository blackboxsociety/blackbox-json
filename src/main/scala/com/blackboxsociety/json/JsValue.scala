package com.blackboxsociety.json

sealed trait JsValue {
  def as[A](implicit c: JsReader[A]): Option[A] = c.read(this)
}

object JsValue {
  def from[A](a: A)(implicit c: JsWriter[A]): JsValue = c.write(a)
}

case class JsBoolean(value: Boolean) extends JsValue {
  override def toString = if (value) "true" else "false"
}

case class JsString(value: String) extends JsValue {
  override def toString = value
}

case class JsInt(value: Int) extends JsValue {
  override def toString = value.toString
}

case class JsFloat(value: Float) extends JsValue {
  override def toString = value.toString
}

case class JsArray(value: Seq[JsValue]) extends JsValue {
  override def toString = "[" + value.map(_.toString).mkString(", ") + "]"
}

case class JsObject(value: Map[String, JsValue]) extends JsValue {
  override def toString = "{" + value.toSeq.map({ n => "\"" + n._1 + "\": " + n._2.toString}).mkString(", ") + "}"
}

case object JsNull extends JsValue {
  override def toString = "null"
}