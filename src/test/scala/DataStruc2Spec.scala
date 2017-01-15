import DataStruc1.{Cons, List, Nil}
import org.scalatest.{FlatSpec, Matchers}

class DataStruc2Spec extends FlatSpec with Matchers {

  val testIntList: List[Int] = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
  val testDoubleList: List[Double] = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))

  it should "print stuff" in {
    println(DataStruc2.foldRight(DataStruc2.testIntList, Nil: List[Int])(Cons(_, _)))
  }



}
