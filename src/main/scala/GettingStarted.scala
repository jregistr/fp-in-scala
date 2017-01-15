import scala.annotation.tailrec

object GettingStarted {

  val anon1: () => Unit = () => println("cool")
  val anon2: () => Unit = () => {
    println("Also cool")
  }

  val successorInt: Int => Int = i => i + 1
  val plusStrings: (String, String) => Int = (a, b) => a.toInt + b.toInt

  val addPie: String => String = _.concat("Pie")

  val a:List[String] = Nil

  println(addPie("Cool"))

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

  def isSorted[T](array: Array[T], lessThan: (T, T) => Boolean): Boolean = {
    def traverse(i: Int): Boolean = {
      if (i + 1 >= array.length) true
      else lessThan(array(i), array(i + 1)) & traverse(i + 1)
    }
    traverse(0)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): (B) => C = (b: B) => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  def unCurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = f compose g // or f(g(a))

}
