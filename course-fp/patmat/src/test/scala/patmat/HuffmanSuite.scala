package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
    val reOcc = List('a', 'b', 'a', 'c', 'c')
    val freqs = List('a' -> 3, 'b' -> 2, 't' -> 7)
    val fork1 = Fork(Leaf('a', 1), Leaf('b', 4), List('a', 'b'), 5)
    val fork2 = Fork(Leaf('c', 2), Leaf('d', 5), List('c', 'd'), 7)
    val fork3 = Fork(fork1, fork2, List('a', 'b', 'c', 'd'), 12)

    val text1: String = "bad"
    val text1EndcodedByFork1: List[Bit] = List[Bit](0, 1, 0, 0, 1, 1)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("Weight of fork 3 tree") {
    new TestTrees {
      assert(weight(fork3) === 12)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("Count occurrences") {
    new TestTrees {
      val occ: List[(Char, Int)] = times(reOcc)
      val exp: List[(Char, Int)] = List('a' -> 2, 'b' -> 1, 'c' -> 2)
      assert(occ === exp)
    }
  }

  test("Make leaf list") {
    new TestTrees {
      val leaves: List[Leaf] = makeOrderedLeafList(freqs)
      val exp: List[Leaf] = List(Leaf('a', 3), Leaf('b', 2), Leaf('t', 7)).sortBy(_.weight)
      assert(leaves === exp)
    }
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("test until") {
    new TestTrees {
      val u: List[CodeTree] = until(singleton, combine)(List(fork1, fork2))
      assert(u.size == 1)
    }
  }

  test("Decode fork3 as 'bad'") {
    new TestTrees {
      val decoded: List[Char] = decode(fork3, text1EndcodedByFork1)
      assert(decoded === text1.toList)
    }
  }

  test("Encode 'bad'") {
    new TestTrees {
      val encoded: List[Bit] = encode(fork3)(text1.toList)
      assert(encoded === text1EndcodedByFork1)
    }
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("Test convert") {
    new TestTrees {
      val res: CodeTable = convert(fork3)
      val exp: List[(Char, List[Bit])] = List('a' -> List(0, 0), 'b' -> List(0, 1),
        'c' -> List(1, 0), 'd' -> List(1, 1))
      assert(res === exp)
    }
  }

  test("Quick Encode 'bad'") {
    new TestTrees {
      val encoded: List[Bit] = quickEncode(fork3)(text1.toList)
      assert(encoded === text1EndcodedByFork1)
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }


}
