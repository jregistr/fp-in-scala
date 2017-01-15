import DataStruc1.{Cons, List, Nil}

import scala.annotation.tailrec

object DataStruc2 {

  val testIntList: List[Int] = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))

  def foldRight[A, B](list: List[A], default: B)(combine: (A, B) => B): B = {
    list match {
      case Nil => default
      case Cons(h, t) => combine(h, foldRight(t, default)(combine))
    }
  }

  @tailrec def foldLeft[A, B](list: List[A], default: B)(combine: (B, A) => B): B = {
    list match {
      case Nil => default
      case Cons(h, t) => foldLeft(t, combine(h, default))(combine)
    }
  }

  def sum(ints: List[Int]): Int = foldRight(ints, 0)(_ + _)

  def product(doubles: List[Double]): Double = foldRight(doubles, 0.0)(_ * _)

  def length[A](list: List[A]): Int = foldRight(list, 0)((_, t) => 1 + t)


  def sum2(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def product2(doubles: List[Double]): Double = foldLeft(doubles, 0.0)(_ * _)

  def length2[A](list: List[A]): Int = foldLeft(list, 0)((l, _) => 1 + l)

}
