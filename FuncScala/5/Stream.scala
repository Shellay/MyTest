package ch5laziness

import Stream._

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

  val ones: Stream[Int] = Cons(() => 1, () => ones)
}


case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

