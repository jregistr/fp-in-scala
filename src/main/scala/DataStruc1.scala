import scala.annotation.tailrec

object DataStruc1 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  val a: List[Int] = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))

  def sum(sequence: List[Int]): Int = {
    sequence match {
      case Nil => 0
      case Cons(h, t) => h + sum(t)
    }
  }

  def tail[A](sequence: List[A]): List[A] = {
    sequence match {
      case Nil => Nil
      case Cons(_, t) => t
    }
  }

  def setHead[A](head: A, sequence: List[A]): List[A] = {
    sequence match {
      case Nil => Cons(head, Nil)
      case Cons(_, t) => Cons(head, t)
    }
  }

  def drop[A](sequence: List[A], n: Int): List[A] = {
    @tailrec def loop(i: Int, current: List[A]): List[A] = {
      if (i == n) current
      else {
        current match {
          case Cons(_, t) => loop(i + 1, t)
          case _ => Nil
        }
      }
    }

    loop(0, sequence)
  }

  def dropWhile[A](sequence: List[A], p: A => Boolean): List[A] = {
    @tailrec def loop(list: List[A]): List[A] = {
      list match {
        case Cons(h, t) =>
          if (p(h)) loop(t)
          else list
        case _ => Nil
      }
    }

    loop(sequence)
  }

  def length[A](sequence:List[A]):Int = {
    sequence match {
      case Nil => 0
      case Cons(_, t) => 1 + length(t)
    }
  }

  def init[A](sequence: List[A]): List[A] = {
    sequence match {
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
      case _ => Nil
    }
  }

  val matchedValue: Int = a match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

}
