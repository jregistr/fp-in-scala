package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    println("------------------------------------------------------------")

    println("Balance or not")
    val t1 = "(if (zero? x) max (/ 1 x))"
    val t2 = "I told him (that it’s not (yet) done). (But he wasn’t listening)"
    val t3 = ":-)"
    val t4 = "())("

    println(s"$t1 isBalanced:${balance(t1.toList)}")
    println(s"$t2 isBalanced:${balance(t2.toList)}")
    println(s"$t3 isBalanced:${balance(t3.toList)}")
    println(s"$t4 isBalanced:${balance(t4.toList)}")

    println("------------------------------------------------------------")
    println("Counting change")
    println(countChange(4, List(1, 2)))
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    (c, r) match {
      case (0, _) => 1
      case (`r`, _) => 1
      case _ => pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    @tailrec def countClosed(chars: List[Char], current: Int): Int = {
      if (current < 0)
        current
      else
        chars match {
          case Nil => current
          case '(' :: Nil => current + 1
          case '(' :: t => countClosed(t, current + 1)
          case ')' :: Nil => current - 1
          case ')' :: t => countClosed(t, current - 1)
          case _ :: Nil => current
          case _ :: t => countClosed(t, current)
        }
    }
    countClosed(chars, 0) == 0
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty)
      0
    else if (money == 0)
      1
    else if (money < 0)
      0
    else countChange(money - coins.head, coins) +  countChange(money, coins.tail)
  }

}
