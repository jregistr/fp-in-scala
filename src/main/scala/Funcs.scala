import scala.annotation.tailrec

object Funcs {

  def getFirst[T](array: Array[T], p: (T) => Boolean): Option[T] = {
    @tailrec def loop(i: Int): Option[T] = {
      if (i >= array.length) None
      else {
        val t = array(i)
        if (p(t)) Some(t)
        else loop(i + 1)
      }
    }

    loop(0)
  }

  def isSorted[T](array: Array[T], compare: (T, T) => Boolean): Boolean = {
    def traverse(i: Int): Boolean = {
      if (i + 1 >= array.length) true
      else compare(array(i), array(i + 1)) & traverse(i + 1)
    }
    traverse(0)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): (B) => C = (b: B) => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  def unCurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

}
