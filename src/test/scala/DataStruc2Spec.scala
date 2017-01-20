import org.scalatest.{FlatSpec, Matchers}

class DataStruc2Spec extends FlatSpec with Matchers {

  val testIntList: List[Int] = List(1,2,3,4,5)
  val testDoubleList: List[Double] = List(1.0, 2.0, 3.0, 4.0, 5.0)

  it should "print stuff" in {
    println(DataStruc2.foldRight(DataStruc2.testIntList, Nil: List[Int])(Cons(_, _)))
  }

  it should "say it's null" in {
    println(List(1,2,4,5))
  }

}
