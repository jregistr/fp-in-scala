import org.scalatest.{FlatSpec, Inside, Matchers}

import scala.util.Random

class FuncsSpec extends FlatSpec with Matchers {

  case class Car(name: String, price: Int)

  def makeCars = Array(
    Car("Camry", 100),
    Car("CRV", 300),
    Car("Camry", 500),
    Car("CX-3", 2000)
  )

  it should "Return some value from the array" in {
    val cars = makeCars
    val search = Funcs.getFirst(cars, (car: Car) => car.name == "Camry" && car.price == 500)
    search.isDefined should be(true)
    Inside.inside(search.get) { case Car(name, price) =>
      name should be("Camry")
      price should be(500)
    }
  }

  it should "Say array is sorted" in {
    Funcs.isSorted(Array(1, 2, 3, 4, 5, 6), (a:Int, b:Int) => b >= a) should be (true)
  }

  it should "Say array of size 1 is sorted" in {
    Funcs.isSorted(Array(Random.nextInt()), (a:Int, b:Int) => b >= a) should be (true)
  }

  it should "Say array of size 0 is sorted" in {
    Funcs.isSorted(Array(), (a:Int, b:Int) => b >= a) should be (true)
  }

  it should "Say it is not sorted" in {
    Funcs.isSorted(Array(1, 3, 2, 4, 5, 6), (a: Int, b: Int) => b >= a) should be(false)
  }

}
