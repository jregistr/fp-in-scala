package objsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {

  trait TestSets {
    val set1 = new Empty
    val set2: TweetSet = set1.incl(new Tweet("a", "a body", 20))
    val set3: TweetSet = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 2)
    val d = new Tweet("d", "d body", 9)
    val set4c: TweetSet = set3.incl(c)
    val set4d: TweetSet = set3.incl(d)
    val set5: TweetSet = set4c.incl(d)
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(_ => true)) == 0)
    }
  }

  test("filter: on set of size 1") {
    new TestSets {
      assert(size(set2.filter(_ => true)) == 1)
    }
  }

  test("filter on set of size + 1") {
    new TestSets {
      assert(size(set4c.filter(_ => true)) == 3)
    }
  }

  test("filter on set of size + 1 Some fail") {
    new TestSets {
      assert(size(set4c.filter(_.user == "a")) == 1)
    }
  }

  test("union of set 2 and set 3") {
    new TestSets {
      val res: TweetSet = set2.union(set3)
      val asCombinedSets: Set[Tweet] = asSet(set2) ++ asSet(set3)
      val resAsSetSet: Set[Tweet] = asSet(res)
      assert(resAsSetSet.size == asCombinedSets.size)
      assert(asCombinedSets.forall(resAsSetSet.contains))
    }
  }

  test("union of set 4d and set 5") {
    new TestSets {
      val res: TweetSet = set4d.union(set5)
      val asCombinedSets: Set[Tweet] = asSet(set4d) ++ asSet(set5)
      val resAsSetSet: Set[Tweet] = asSet(res)
      assert(resAsSetSet.size == asCombinedSets.size)
      assert(asCombinedSets.forall(resAsSetSet.contains))
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("descending: set5") {
    new TestSets {
      set5.foreach(a => print(s" ${a.user}"))
      val trends: TweetList = set5.descendingByRetweet
      trends.foreach(t => print(s" ${t.user}"))

      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }

}
