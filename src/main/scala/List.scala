
sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](items:A*): List[A] = {
    items.toList match {
      case scala.Nil => Nil
      case h::t => Cons(h, apply(t:_*))
    }
  }
}