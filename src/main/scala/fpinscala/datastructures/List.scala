package fpinscala.datastructures

sealed trait List[+A] {
  override def toString: String =
    this match {
      case Nil         => "Nil"
      case Cons(x, xs) => s"Cons($x, $xs)"
    }
}
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](xs: A*): List[A] =
    if (xs.isEmpty) Nil
    else Cons(xs.head, apply(xs.tail: _*))

  def sum(list: List[Int]): Int = list match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(list: List[Double]): Double = list match {
    case Nil         => 1
    case Cons(0, _)  => 0
    case Cons(x, xs) => x * product(xs)
  }

  def tail[A](list: List[A]): List[A] = list match {
    case Nil         => Nil
    case Cons(_, xs) => xs
  }

  def setHead[A](list: List[A], a: A): List[A] = list match {
    case Nil         => Nil
    case Cons(_, xs) => Cons(a, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil         => Nil
    case Cons(_, xs) => if (n - 1 == 0) xs else drop(xs, n - 1)
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil         => Nil
    case Cons(x, xs) => if (f(x)) dropWhile(xs)(f) else l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) => Nil
    case Cons(x, xs)  => Cons(x, init(xs))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil         => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil         => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def foldLeft2[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil         => z
    case Cons(x, xs) => foldLeft2(xs, f(x, z))(f)
  }

  def sum2(ns: List[Int]): Int = foldRight(ns, 0)(_ + _)
  def product2(ns: List[Int]): Int = foldRight(ns, 1)(_ * _)

//  def divide[A >: Int](ns: List[A]): A = foldRight(ns, 1.0)(_ / _)

  def foldR[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => {
        f(x, foldR(xs, z)(f))
      }
    }

  def foldL[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil         => z
      case Cons(x, xs) => foldL(xs, f(z, x))(f)
    }
  }
}
