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

  def increment(xs: List[Int]): List[Int] =
    foldRight(xs, Nil:List[Int])((y, ys) => Cons(y + 1, ys))

  def doubleToString(ds: List[Double]): List[String] =
    foldRight(ds, Nil:List[String])((x, xs) => Cons(x.toString, xs))

  def map[A, B](xs: List[A])(f: A => B): List[B] =
    foldRight(xs, Nil:List[B])((y, ys) => Cons(f(y), ys))

  def filter[A](xs: List[A])(f: A => Boolean): List[A] =
    foldRight(xs, Nil:List[A])((y, ys) => if (f(y)) Cons(y, ys) else ys)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil:List[B])((x, xs) => appendR(f(x), xs))

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => if (f(x)) List(x) else Nil)

  def addPairWise(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addPairWise(xs, ys))
  }

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
  }

  def startsWith[A](l: List[A], pre: List[A]): Boolean = (l, pre) match {
    case (_, Nil) => true
    case (Cons(x, xs), Cons(y, ys)) if x == y => startsWith(xs, ys)
    case _ => false
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => false
    case Cons(x, xs) if startsWith(sup, sub) => true
    case Cons(x, xs) => hasSubsequence(xs, sub)
  }

  def apply[A](xs: A*): List[A] =
    if (xs.isEmpty) Nil
    else Cons(xs.head, apply(xs.tail: _*))
}
