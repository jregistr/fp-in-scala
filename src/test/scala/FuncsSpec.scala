import org.scalatest.{FlatSpec, Inside, Matchers}

import scala.util.Random

class FuncsSpec extends FlatSpec with Matchers {

  case class Car(name: String, price: Int)

  val cars = Array(
    Car("Camry", 100),
    Car("CRV", 300),
    Car("Camry", 500),
    Car("CX-3", 2000)
  )

  val people = Map(
    "Jeff" -> 5000,
    "Jack" -> 1000,
    "Peter" -> 50,
    "Bob" -> 400
  )

  it should "Return some value from the array" in {
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

  it should "make some delicious curry" in {
    val stringFormat = "%s has a nice %s that costs %d green backs"
    val camry = cars(0)
    val curry = Funcs.curry((car:Car, owner:String) => stringFormat.format(owner, car.name, car.price))
    val formatFunc: (String) => String = curry(camry)
    val formatted = formatFunc("Jeff")
    println(formatted)
    formatted should be (stringFormat.format("Jeff", camry.name, camry.price))
  }

  it should "make a very nice composed function" in {
    // A:Person, B:Int(Budget), C:Cars
    def canBuy:Int => List[Car] = budget => cars.filter(_.price <= budget).toList
    def walletOf:String => Int = person => people(person)

    val carsForPerson: (String) => List[Car] = Funcs.compose(canBuy, walletOf)
    println(carsForPerson("Jeff"))
    println(carsForPerson("Jack"))
    println(carsForPerson("Peter"))
    println(carsForPerson("Bob"))

  }

}
