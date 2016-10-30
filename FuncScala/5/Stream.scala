// package ch5laziness
// import Stream._

sealed trait Stream[+A] {

  // def headOption: Option[A] = this match {
  //   case Empty => None
  //   case Cons(h, t) => Some(h())
  // }

  def foldRight[B](b: => B)(f: (A, => B) => B): B = this match {
    case Empty => b
    case Cons(h, t) => f(h(), t().foldRight(b)(f))
  }

  def toList: List[A] = {
    foldRight(Nil: List[A])(_ :: _)
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(hlz, tlz) => if (n == 0) Empty else Cons(hlz, () => tlz().take(n - 1))
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)(p(_) && _)

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty)((a, as) => if (p(a)) Cons(()=>a, ()=>as) else Empty)

  def headOption: Option[A] =
    foldRight[Option[A]](None)((h, _) => Some(h))

  def map[B](f: A => B) =
    foldRight[Stream[B]](Empty)((h, t) => Cons(()=>f(h), ()=>t))

  def filter(p: A => Boolean) =
    foldRight[Stream[A]](Empty)((h, t) => if (p(h)) Cons(()=>h, ()=>t) else t)

  def append[B >: A](bs: => Stream[B]): Stream[B] =
    foldRight[Stream[B]](bs)((h, t) => Cons(()=>h, ()=>t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight[Stream[B]](Empty)((h, t) => f(h) append t)

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

}


case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


object Main {

  val ones: Stream[Int] = Cons(() => 1, () => ones)

  /*
  def constant[A](a: A): Stream[A] =
    Cons(()=>a, ()=>constant(a))

  def from(n: Int): Stream[Int] =
    Cons(()=>n, ()=>from(n+1))

  def fib: Stream[Int] = {
    def f(a1: Int, a2: Int): Stream[Int] =
      Cons(()=>a1, ()=>f(a2, a1+a2))
    f(0, 1)
  }
   */

  def unfold[A, S](s: S)(f: S => Option[(A, S)]): Stream[A] = {
    /* Corecursive */
    f(s) match {
      case None => Empty
      case Some((a, s1)) => Cons(()=>a, ()=>unfold(s1)(f))
    }
  }

  def constant[A](a: A): Stream[A] =
    unfold[A, Int](0)(_ => Some((a, 0)))

  def from(n: Int): Stream[Int] =
    unfold[Int, Int](0)(i => Some((i, i+1)))

  def fib: Stream[Int] = 
    unfold((0, 1))(_ match {case (a1, a2) => Some((a1, (a2, a1+a2)))})
  
  def mapUF[P, Q](ps: Stream[P])(f: P => Q) =
    unfold[Q, Stream[P]](ps)(_ match {
      case Empty => None
      case Cons(plz, pslz) => Some((f(plz()), pslz())) // : Option[(Q, S=Stream[P])]
    })

  def takeUF[A](as: Stream[A])(n: Int) =
    unfold[A, (Int, Stream[A])]((n, as))(_ match {
      case (0, _) => None
      case (_, Empty) => None
      case (k, Cons(alz, aslz)) => Some((alz(), (k-1, aslz())))
    })

  def takeWhileUF[A](as: Stream[A])(p: A => Boolean) =
    unfold[A, Stream[A]](as)(_ match {
      case Empty => None
      case Cons(alz, aslz) => alz() match {
        case a if p(a) => Some(a, aslz())
        case _ => None
      }
    })

  def zipWith[A, B, C](as: Stream[A], bs: Stream[B])(f: (A, B) => C) =
    unfold[C, (Stream[A], Stream[B])]((as, bs))(_ match {
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(alz, aslz), Cons(blz, bslz)) => Some(
        (f(alz(), blz()), (aslz(), bslz()))
      )
    })

  def zipAll[A, B](as: Stream[A], bs: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold[(Option[A], Option[B]), (Stream[A], Stream[B])]((as, bs))(_ match {
      case (Empty, Empty) => None
      case (Empty, Cons(b, bb)) => Some( ((None, Some(b())), (Empty, bb())) )
      case (Cons(a, aa), Empty) => Some( ((Some(a()), None), (aa(), Empty)) )
      case (Cons(a, aa), Cons(b, bb)) => Some( ((Some(a()), Some(b())), (aa(), bb())) )
    })

  def main(argv: Array[String]) = {
    // println(ones.take(5).toList)
    // println(fib.take(15).toList)
    // println(mapUF(fib.take(15))(_ * 10).toList)
    println(takeUF(mapUF(fib)(_ * 10))(15).toList)
    println(takeWhileUF(mapUF(fib)(_ * 10))(_ < 10000).toList)
  }

}
