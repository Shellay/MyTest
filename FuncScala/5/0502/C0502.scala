sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }
  def toList: List[A] = this match {
    case Empty => List()
    case Cons(hlz, tlz)=> (hlz() :: tlz().toList)
  }
  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(hlz, tlz) => if (n == 0) Empty else Cons(hlz, () => tlz().take(n-1))
  }
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(hlz, tlz) => {
      lazy val h = hlz() // memo
      if (p(h)) Cons(() => h, () => tlz().takeWhile(p))
      else tlz()
    }
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t1: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}

