val w1 = "these"
val w2 = "Robert"

val s1 = "This is a super cool and I mean cool sentence"

val s2 =
  """If you know your enemy and you know yourself you need
     | not fear the result of a hundred battles if you know
     | your enemy and not yourself for every victory gained you
     | will also suffer a defeat
  """.stripMargin

w1.toList.groupBy(char => w1.count(_ == char))

w2.toLowerCase().toList.groupBy(char => w2.count(_ == char))

val s2Lower = s2.toLowerCase

s2Lower.groupBy(c => s2Lower.count(_ == c))