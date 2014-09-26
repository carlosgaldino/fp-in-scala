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

  def foldRight[A, B](xs: List[A], z: B)(f: (A, B) => B): B = xs match {
    case Nil => z
    case Cons(y, ys) => f(y, foldRight(ys, z)(f))
  }

  def length[A](xs: List[A]): Int =
    foldRight(xs, 0)((_, acc) => 1 + acc)

  @annotation.tailrec
  def foldLeft[A, B](xs: List[A], z: B)(f: (B, A) => B): B = xs match {
    case Nil => z
    case Cons(y, ys) => foldLeft(ys, f(z, y))(f)
  }

  def sumL(xs: List[Int]): Int =
    foldLeft(xs, 0)(_ + _)

  def productL(xs: List[Double]): Double =
    foldLeft(xs, 1.0)(_ * _)

  def lengthL[A](xs: List[A]): Int =
    foldLeft(xs, 0)((acc, _) => 1 + acc)

  def reverse[A](xs: List[A]): List[A] =
    foldLeft(xs, List[A]())((acc, x) => Cons(x, acc))

  def foldLeftViaRight[A, B](xs: List[A], z: B)(f: (B, A) => B): B =
    foldRight(xs, (b:B) => b)((a, g) => b => g(f(b, a)))(z)

  def foldRightViaLeft[A, B](xs: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(xs), z)((b, a) => f(a, b))

  def appendL[A](xs: List[A], ys: List[A]): List[A] =
    foldLeft(reverse(xs), ys)((b, a) => Cons(a, b))

  def appendR[A](xs: List[A], ys: List[A]): List[A] =
    foldRight(xs, ys)((a, b) => Cons(a, b))

  def concat[A](xs: List[List[A]]): List[A] =
    foldRight(xs, Nil:List[A])(appendR)

  def apply[A](xs: A*): List[A] =
    if (xs.isEmpty) Nil
    else Cons(xs.head, apply(xs.tail: _*))
}
