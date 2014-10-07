package fpinscala.laziness

import Stream._

sealed trait Stream[+A] {
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] =
    if (n > 0) this match {
      case Cons(h, t) if n == 1 => cons(h(), Stream.empty)
      case Cons(h, t) => cons(h(), t().take(n - 1))
      case _ => Stream.empty
    }
    else Stream.empty

  def drop(n: Int): Stream[A] = {
    @annotation.tailrec
    def go(st: Stream[A], n: Int): Stream[A] =
      if (n <= 0) st
      else st match {
        case Cons(_, t) => go(t(), n - 1)
        case _ => Stream()
      }

    go(this, n)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

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
