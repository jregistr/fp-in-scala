import DataStruc1.{Cons, Nil, List}
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.Inside.inside

class DataStruc1Spec extends FlatSpec with Matchers {

  val testIntList: List[Int] = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
  val testDoubleList: List[Double] = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))

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
