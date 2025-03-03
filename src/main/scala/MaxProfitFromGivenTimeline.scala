package oz.sandbox
import scala.collection.ArrayOps._

object MaxProfitFromGivenTimeline {

  val timeline = Array(
    2,
    30,
    15,
    1,
    10,
    8,
    25,
    80,
    78,
  )

  val expected = 79


  def main(args: Array[String]): Unit = {
    var maxProfit = 0
    var minPrice = Int.MaxValue

    def getMaxProfit2(timeline: Array[Int]): Int = timeline match {
      case Array() => 0
      case Array(first, rest @ _*) =>
        minPrice = Math.min(minPrice, first)
        Math.max(getMaxProfit2(rest.toArray), first - minPrice)
    }

    maxProfit = getMaxProfit2(timeline)
    assert(maxProfit == expected,  s"Expected $expected but got $maxProfit")
    println(maxProfit)
  }


}
