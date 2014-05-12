package com.blackboxsociety.json

trait JsWriter[A] {
  def write(a: A): JsValue
}

object JsWriter {

  implicit object booleanJsWriter extends JsWriter[Boolean] {
    override def write(a: Boolean): JsValue = JsBoolean(a)
  }

  implicit object stringJsWriter extends JsWriter[String] {
    override def write(a: String): JsValue = JsString(a)
  }

  implicit object intJsWriter extends JsWriter[Int] {
    override def write(a: Int): JsValue = JsInt(a)
  }

  implicit object floatJsWriter extends JsWriter[Float] {
    override def write(a: Float): JsValue = JsFloat(a)
  }

  implicit def listJsWriter[A](implicit c: JsWriter[A]) = new JsWriter[List[A]] {
    override def write(a: List[A]): JsValue = JsArray(a.map({ n => c.write(n) }))
  }

  implicit def mapJsWriter[A](implicit c: JsWriter[A]) = new JsWriter[Map[String, A]] {
    override def write(a: Map[String, A]): JsValue = JsObject(a.mapValues({n => c.write(n)}))
  }

}
