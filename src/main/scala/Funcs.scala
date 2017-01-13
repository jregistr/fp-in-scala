import scala.annotation.tailrec

object Funcs {

  def getFirst[T](array:Array[T], p:(T) => Boolean) : Option[T] = {
    @tailrec def loop(i:Int):Option[T] = {
      if(i >= array.length) None
      else {
        val t = array(i)
        if (p(t)) Some(t)
        else loop(i + 1)
      }
    }
    loop(0)
  }

  def isSorted[T](array: Array[T], compare:(T,T) => Boolean):Boolean = {
    def loop(i:Int):Boolean = {
      if(i + 1 >= array.length) true
      else compare(array(i), array(i + 1)) & loop(i + 1)
    }
    loop(0)
  }

}
