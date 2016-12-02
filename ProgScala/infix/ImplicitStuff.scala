/**
  * When invoking a non-member method `fooo` of some object `a`, if
  * there in current scope exists a method with this non-member name,
  * it is then checked whether `a` can be casted to the owner-type `B`
  * of this method implicitly. If OK, then construct a `B` instance
  * with object `a` getting `b$` and `b$.fooo` gets invoked. */

implicit class Logical(val b: Boolean) extends AnyVal {

  def ==>>(that: Boolean) = !this.b || that
  // (false ==>> true) == true
  // (true ==>> false) == false

}

/**
  * This may be useful to make frameworks!
  * + Test (descriptive) frameworks;
  * + Design by Contract?
  *   - http://www.scala-lang.org/old/node/6958
  *   - simulated by dependent-type
  */

implicit class InSth[A](val x: A) extends AnyVal {

  def in(that: List[A]): Boolean =
    that.find(x == _) match {
      case Some(_) => true
      case _ => false
    }

}


class KB {

}
