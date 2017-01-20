package p1.ch3.tree


sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def apply[A](values: A*): Tree[A] = {
    values.toList match {
      case scala.Nil => throw new IllegalArgumentException("need at least 1")
      case h :: Nil => Leaf(h)
      case h :: t => Branch(apply(h), apply(t: _*))
    }
  }

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def debt[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (debt(l) max debt(r))
  }

  def map[A, B](tree: Tree[A])(trans: A => B): Tree[B] = tree match {
    case Leaf(v) => Leaf(trans(v))
    case b: Branch[A] => map(b)(trans)
  }

}