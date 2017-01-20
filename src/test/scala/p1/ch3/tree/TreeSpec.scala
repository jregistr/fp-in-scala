package p1.ch3.tree

import org.scalatest.{FlatSpec, Matchers}


class TreeSpec extends FlatSpec with Matchers {

  it should "give the correct size for tree" in {
    val seq = List(1, 2, 3, 4, 5)
    Tree.size(Tree(seq:_*)) should be(seq.length)
  }

}
