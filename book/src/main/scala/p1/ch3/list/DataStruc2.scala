package p1.ch3.list

import scala.annotation.tailrec


object DataStruc2 {

  val testIntList: List[Int] = List(1, 2, 3, 4, 5)

  def foldRight[A, B](list: List[A], default: B)(combine: (A, B) => B): B = {
    list match {
      case Nil => default
      case Cons(h, t) => combine(h, foldRight(t, default)(combine))
    }
  }

  @tailrec def foldLeft[A, B](list: List[A], default: B)(combine: (B, A) => B): B = {
    list match {
      case Nil => default
      case Cons(h, t) => foldLeft(t, combine(default, h))(combine)
    }
  }

  def sum(ints: List[Int]): Int = foldRight(ints, 0)(_ + _)

  def product(doubles: List[Double]): Double = foldRight(doubles, 0.0)(_ * _)

  def length[A](list: List[A]): Int = foldRight(list, 0)((_, t) => 1 + t)

  def sum2(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def product2(doubles: List[Double]): Double = foldLeft(doubles, 0.0)(_ * _)

  def length2[A](list: List[A]): Int = foldLeft(list, 0)((l, _) => 1 + l)

  def reverse[A](list: List[A]): List[A] = foldLeft(list, List[A]())((b, a) => Cons(a, b))

  def append[A](a: List[A], b: List[A]): List[A] = foldRight(a, b)(Cons(_, _))

  def flatten[A](lists: List[List[A]]): List[A] = foldRight(lists, Nil: List[A])(append)

  def upOne(ints: List[Int]): List[Int] = foldRight(ints, Nil: List[Int])((item, list) => Cons(item + 1, list))

  def doublesToStrings(doubles: List[Double]): List[String] = foldRight(doubles, Nil: List[String])((item, list) => Cons(item.toString, list))

  def map[A, B](list: List[A])(trans: A => B): List[B] = foldRight(list, Nil: List[B])((a, b) => Cons(trans(a), b))

  def filter[A](list: List[A])(f: A => Boolean): List[A] = foldRight(list, Nil: List[A])((item, list) => if (f(item)) Cons(item, list) else list)

  def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = foldRight(list, Nil: List[B])((h, t) => append(f(h), t))

  def merge[A, B, C](first: List[A], second: List[B])(combine: (A, B) => C): List[C] = (first, second) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(f, t1), Cons(s, t2)) => Cons(combine(f, s), merge(t1, t2)(combine))
  }

}
