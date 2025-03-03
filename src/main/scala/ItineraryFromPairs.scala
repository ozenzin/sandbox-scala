package oz.sandbox

import scala.annotation.tailrec

object ItineraryFromPairs {

  def main(args: Array[String]): Unit = {
    val pairs = List(("SFO", "HKO"), ("YYZ", "SFO"), ("YUL", "YYZ"), ("HKO", "ORD"))
    val itinerary = findItinerary(pairs)
    println(s"$itinerary")
  }

  def findItinerary(pairs: List[(String, String)]): List[String] = {
    val map = pairs.toMap
    val (starts, ends) = pairs.unzip
    val start = (starts diff ends).head

    @tailrec
    def itinerary(st: String, itin: List[String]): List[String] =
      map.get(st) match {
        case Some(end) => itinerary(end, itin :+ end)
        case _ => itin
      }
    @tailrec
    def itinerary2(itin: List[String]): List[String] =
      if (itin.length < pairs.length+1) itin match {
        case Nil => Nil
        case head::_ => itinerary2(map(head) +: itin)
      } else itin


    println(itinerary(start, List(start)))
    itinerary2(List(start)).reverse
  }
}
