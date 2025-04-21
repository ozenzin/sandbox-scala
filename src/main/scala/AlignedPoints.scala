package oz.sandbox

/**
 * Given their coordinates find max number of points connected with one line.
 */
object AlignedPoints {
  /*
  To find the line formula (ax + by + c = 0) given two points (x₁,y₁) and (x₂,y₂), you can use these coefficients: [1]

    a = y₁ - y₂
    b = x₂ - x₁
    c = x₁*y₂ - y₁*x₂
   */

  /*
  We can start with one point and "draw" a line (calculate a, b, c) to another point, then checking the rest points
  if they belong to the same line (considering (a, b, c) scalar multiples). All matching points will be put in a List
  and excluded from the next iteration. Once all points exhausted we would compare the created lists lengths.
   */
/*
  Better: points.combinations(2).map { case Seq((x1, y1), (x2, y2)) =>
    val a = y1 - y2
    val b = x2 - x1
    val c = x1 * y2 - y1 * x2
  }
  .foldLeft(Seq[Seq[(Int, Int, Int)]]().empty){
    case (lines, (a, b, c)) =>
      val found = lines.indexWhere{
        //two lines are same
      }
      if (found < 0) lines :+ Seq((a, b, c))
      else lines.updated(found, (a, b, c) +: lines(found))
  }
  .maxBy(_.length)
 */

  type Point = Tuple2[Int, Int]

  val points: Seq[Point] = Seq(
    (1, 1), (1, 2), (1, 3), (1, 4), (2, 1), (2, 2), (2, 3), (2, 4), (3, 1), (3, 2), (3, 3), (3, 4), (4, 1), (4, 2), (4, 3), (4, 4), (5, 5), (5, 6), (5, 7), (5, 8), (6, 5), (6, 6), (6, 7), (6, 8), (7, 5), (7, 6), (7, 7), (7, 8), (8, 5), (8, 6), (8, 7), (8, 8))


  def main(args: Array[String]): Unit = {
    val twoPoints: Seq[Seq[Point]] = points.combinations(2).toSeq
    val segments: Seq[(Int, Int, Int)] = twoPoints.map {
      case Seq(p1, p2) => ((p1._2 - p2._2), (p2._1 - p1._1), (p1._1 * p2._2 - p1._2 * p2._1))
    }

    val lines = segments.foldLeft(Seq[Seq[(Int, Int, Int)]]().empty){
      case (lines, (a, b, c)) =>
        val lineContainingSegmentIndex = lines.indexWhere{
          case (a1, b1, c1) :: _ if (a * b1 == a1 * b && b * c1 == b1 * c && a * c1 == a1 * c) => true
          case _ => false
        }
        if (lineContainingSegmentIndex < 0) lines :+ Seq((a, b, c))
        else lines.updated(lineContainingSegmentIndex, (a, b, c) +: lines(lineContainingSegmentIndex))
    }

    val maxLine = lines.maxBy(_.length)
    val (a, b,c) = maxLine.head
    val maxLinePoints = points.filter { p =>
        a * p._1 + c == -b * p._2 // a * x + c == -b * y
      }

    println(s"${maxLinePoints.length} points found on line with segments $maxLine:\n $maxLinePoints")
  }

}