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

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) if p(h()) => t().forAll(p)
    case Empty => true
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def takeWhileViaFold(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else b)

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  def append[B >: A](st: => Stream[B]): Stream[B] =
    foldRight(st)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a).append(b))

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), n) if n == 1 => Some((h(), (empty, n - 1)))
      case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s)) {
      case (Cons(h, t), Cons(x, y)) => Some((f(h(), x()), (t(), y())))
      case _ => None
    }

  def startsWith[B >: A](s: Stream[B]): Boolean = (this, s) match {
    case (_, Empty) => true
    case (Cons(h, t), Cons(x, y)) if h() == x() => t() startsWith y()
    case _ => false
  }

  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(empty)

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

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] =
      cons(a, go(b, a + b))

    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, b)) => cons(a, unfold(b)(f))
      case None => empty
    }

  def fibsViaUnfold: Stream[Int] =
    unfold((0, 1)) { case (a, b) => Some((a, (b, a + b))) }

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some(a, a))

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(x => Some((x, x + 1)))

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
