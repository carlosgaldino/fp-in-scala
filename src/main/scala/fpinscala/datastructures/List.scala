package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(xs: List[Int]): Int = xs match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(xs: List[Double]): Double = xs match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, xs) => xs
  }

  def setHead[A](xs: List[A], x: A): List[A] = xs match {
    case Nil => sys.error("set head on empty list")
    case Cons(_, ys) => Cons(x, ys)
  }

  def drop[A](xs: List[A], n: Int): List[A] = {
    if (n <= 0) xs
    else xs match {
      case Nil => Nil
      case Cons(_, ys) => drop(ys, n - 1)
    }
  }

  def dropWhile[A](xs: List[A], f: A => Boolean): List[A] = xs match {
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else Cons(x, xs)
    case _ => xs
  }

  def init[A](xs: List[A]): List[A] = xs match {
    case Cons(_, Nil) => Nil
    case Cons(y, ys) => Cons(y, init(ys))
  }

  def apply[A](xs: A*): List[A] =
    if (xs.isEmpty) Nil
    else Cons(xs.head, apply(xs.tail: _*))
}
