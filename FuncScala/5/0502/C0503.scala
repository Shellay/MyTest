sealed trait Stream[+A] {
  // def headOption: Option[A] = this match {
  //   case Empty => None
  //   case Cons(h, t) => Some(h())
  // }
  def toList: List[A] = this match {
    case Empty => List()
    case Cons(hlz, tlz)=> (hlz() :: tlz().toList)
  }
  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(hlz, tlz) => if (n == 0) Empty else Cons(hlz, () => tlz().take(n-1))
  }
  // def takeWhile(p: A => Boolean): Stream[A] = this match {
  //   case Empty => Empty
  //   case Cons(hlz, tlz) => {
  //     lazy val h = hlz() // memo
  //     if (p(h)) Cons(() => h, () => tlz().takeWhile(p))
  //     else tlz()
  //   }
  // }
  // def exists(p: A => Boolean): Boolean = this match {
  //   case Cons(hlz, tlz) => p(hlz()) || tlz().exists(p)
  //   case _ => false
  // }
  def foldRight[B](b: => B)(f: (A, => B) => B): B = this match {
    case Cons(hlz, tlz) => f(hlz(), tlz().foldRight(b)(f))
    case Empty => b
  }
  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, blz) => p(a) || blz)
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, blz) => p(a) && blz)
  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, salz) => {
      if (p(a)) Cons(() => a, () => salz) else salz
    })
  def headOption: Option[A] =
    foldRight(None: Option[A])((a, salz) => Some(a))
  def map[B](f: A => B): Stream[B] =
    foldRight(Empty: Stream[B])((a, salz) => Cons(() => f(a), () => salz))
  def filter(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, salz) => if (p(a)) Cons(() => a, () => salz) else salz)
  // When B supers A -> Contravariance (reverse along the hierarchy):
  // appending and casting resulted Stream[A] to Stream[B] legally!
  def append[B >: A](as: => Stream[B]): Stream[B] =
    foldRight(as)(
      (a, salz) => Cons(() => a, () => salz))
  // def flatMap[B, C](bs: => Stream[B])(f: (A, B) => C): Stream[C] = {
  //   def _f(a: A, cs: => Stream[C]): Stream[C] =
  //     bs.map(f(a, _)).append(cs)
  //   foldRight(Empty: Stream[C])(_f)
  // }
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B])((a, bs) => f(a) append bs)
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

