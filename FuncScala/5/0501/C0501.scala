object C0501 {

  def main(argv: Array[String]) {
    println("Hello.")

    println(if2(false, sys.error("fail"), 3))
  }


  def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A = {
    if (cond) onTrue else onFalse
  }

  def maybeTwice2(b: Boolean, i: => Int) = {
    lazy val j = i
    if (b) j+j else 0
  }


}
