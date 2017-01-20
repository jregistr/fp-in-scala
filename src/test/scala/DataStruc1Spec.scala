import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.Inside.inside

class DataStruc1Spec extends FlatSpec with Matchers {

  val testIntList: List[Int] = List(1,2,3,4,5)
  val testDoubleList: List[Double] = List(1.0, 2.0, 3.0, 4.0, 5.0)

  it should "print the match" in {
    println(DataStruc1.matchedValue)
  }

  it should "return correct length for list" in {
    val length = DataStruc1.length(testIntList)
    length should be(5)
  }

  it should "give a list containing all but last" in {
    val firsts = DataStruc1.init(testIntList)
    inside(firsts) {
      case Nil => fail
      case Cons(1, Cons(2, Cons(3, Cons(4, Nil)))) => succeed
      case _=> fail
    }
  }

}
