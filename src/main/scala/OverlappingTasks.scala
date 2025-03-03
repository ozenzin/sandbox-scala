package oz.sandbox

/*
Define a task had a starting time and end time, each task need to be executed in one slot(like a CPU).
Given a list of tasks, at least how many execution slots are needed?

Example 1:
Input: [[0, 30],[5, 10],[15, 20]]
Output: 2
Example 2:
Input: [[7,10],[2,4]]
Output: 1
*/


object OverlappingTasks {

  val tasks: List[(Int, Int)]= List(
    (0, 30),
    (5, 10),
    (15, 20),
    (7, 10),
    (2, 6)
  )

  def main(args: Array[String]): Unit = {
    val expected = 3
    val slots = Solution.minSlots(tasks)
    assert(expected == slots, s"expected $expected slots, but got $slots")
  }


  object Solution {

    /*
    Sorting all starts and ends together, but marking them differently:
    +1 for start, -1 for end.
    Then we can just go through the sorted list (in chronological order) and keep track of the current number of tasks
    by +1 for starts and -1 for ends. While doing that we record max.

    Time complexity: O(n log n) due to sorting, space is O(n), where n is the number of tasks.
     */
    def minSlots(tasks: List[(Int, Int)]): Int = {
      if (tasks.isEmpty) return 0
      tasks.flatMap(task => List((task._1, 1), (task._2, -1))).sortBy(_._1).foldLeft((0, 0)) {
        case ((max, current), (_, ind)) =>
          val newCurrent = current + ind
          (if (newCurrent > max) newCurrent else max, newCurrent)
      }._1
    }
  }
}