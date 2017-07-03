import scala.annotation.tailrec

val l1 = List("a", "a", "a", "b", "b", "c", "d", "d", "b", "b")

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: _ =>
    val (h, t) = xs.span(_ == x)
    h :: pack(t)
}

l1.span(_ == "z")

pack(l1)

def encode[T](xs: List[T]): List[(T, Int)] = xs match {
  case Nil => Nil
  case x :: _ =>
    val (h, t) = xs.span(_ == x)
    (h.head, h.size) :: encode(t)
}

encode(List("a", "a", "a", "b", "c", "c", "a"))

def tailEncode[T](xs: List[T]): List[(T, Int)] = {
  @tailrec def loop(acc: List[(T, Int)], list: List[T]): List[(T, Int)] = list match {
    case Nil => acc
    case x :: _ =>
      val (h, t) = list span (_ == x)
      loop(acc :+ h.head -> h.size, t)
  }

  loop(Nil, xs)
}

tailEncode(List("a", "a", "a", "b", "c", "c", "a"))


