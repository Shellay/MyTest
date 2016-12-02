abstract class Element {
  def contents: Array[String] // virtual
  val height = contents.length
  val width =
    if (height == 0) 0 else contents(0).length
}


/** extending */
class ArrayElement(conts: Array[String]) extends Element {
  // overriding with method 
  // def contents: Array[String] = conts

  // overriding with field
  val contents: Array[String] = conts
}

/** parametric field */
class ArrayElement1(
  val contents: Array[String]
) extends Element


class Cat {
  val dangerous = false
}
class Tiger(
  override val dangerous: Boolean,
  private var age: Int
) extends Cat


/** invoking base ctor */
class LineElement(s: String) extends ArrayElement(Array(s)) {
  override def width = s.length
  override df height = 1
}
