package oz.sandbox

import scala.collection.mutable

/**
 * Given a set of candidate numbers (candidates) (without duplicates) and a target number (target),
 * find all unique combinations in candidates where the candidate numbers sums to target.
 *
 * The same repeated number may be chosen from candidates unlimited number of times.
 *
 * Note:
 *
 * All numbers (including target) will be positive integers.
 * The solution set must not contain duplicate combinations.
 * Example 1:
 *
 * Input: candidates = [2,3,6,7], target = 7,
 * A solution set is:
 * [
 * [7],
 * [2,2,3]
 * ]
 * Example 2:
 *
 * Input: candidates = [2,3,5], target = 8,
 * A solution set is:
 * [
 * [2,2,2,2],
 * [2,3,3],
 * [3,5]
 * ]
 */
object CombinationSum {

  val target: Int = 8

  val ns: List[Int] = List(2, 3, 5)//sorted

  val result = new mutable.ArrayBuffer[Seq[Int]]()

  def main(args: Array[String]): Unit = {

    combinations(List[Int]().empty, 0)

    result.foreach{ comb =>
      print(comb.mkString(" + "))
      require(target == comb.sum)
      println(s" = $target")
    }

  }

  def combinations(cand: List[Int], offset: Int): Unit = {
    val sum = cand.sum
    if (sum == target) {
      result += cand
    } else if (sum < target) for ( i <- offset until ns.length) {
      combinations(cand :+ ns(i), i)
    }
  }
}
