package w4

object w4 {

}


abstract class Nat {
  def isZero: Boolean

  def predecessor: Nat

  def successor: Nat = new Succ(this)

  def +(that: Nat): Nat

  def -(that: Nat): Nat
}

object Zero extends Nat {
  override def isZero: Boolean = true

  override def predecessor: Nat = throw new NoSuchElementException

  override def +(that: Nat): Nat = that

  override def -(that: Nat): Nat = that match {
    case Zero => this
    case _ => throw new NoSuchElementException
  }
}

class Succ(n: Nat) extends Nat {
  override def isZero: Boolean = false

  override def predecessor: Nat = n

  override def +(that: Nat): Nat = new Succ(n + that)

  override def -(that: Nat): Nat = that match {
    case Zero => this
    case _ => n - that.predecessor
  }
}

trait List[+T] {
  def isEmpty: Boolean

  def head: T

  def tail: List[T]
}

case object Nil extends List[Nothing] {
  override def isEmpty: Boolean = true

  override def head: Nothing = throw new NoSuchElementException

  override def tail: List[Nothing] = throw new NoSuchElementException
}

class Cons[+T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty: Boolean = false
}

object List {
  def apply[T](): List[T] = Nil

  def apply[T](f: T): List[T] = new Cons[T](f, Nil)
}