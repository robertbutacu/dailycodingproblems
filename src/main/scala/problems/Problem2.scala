package problems


import scala.annotation.tailrec
import scala.collection.immutable.HashSet

/*
Given a list of numbers and a number k, return whether any two numbers from the list add up to k.

For example, given [10, 15, 3, 7] and k of 17, return true since 10 + 7 is 17.

Bonus: Can you do this in one pass?
 */

object Problem2 extends App with TimingHelpers {
  def summed(numbers: List[Double], k: Double): Boolean = {
    @tailrec
    def go(remaining: List[Double], generated: List[Double]): Boolean = {
      remaining match {
        case Nil       => false
        case h :: tail =>
          generated.find(p => p - h == 0) match {
            case None    => go(tail, generated :+ (k - h))
            case Some(e) =>
              println(s"Found pair: ${k - e} and $h ")
              true
          }
      }
    }

    go(numbers, List.empty)
  }

  def summedHashed(numbers: List[Double], k: Double): Boolean = {
    @tailrec
    def go(left: List[Double], hashed: HashSet[Double]): Boolean = {
      left match {
        case Nil       => false
        case h :: tail =>
          if(hashed.contains(h)) {
            println(s"Found pair: $h and ${k - h}")
            true
          }
          else go(tail, hashed + (k - h))
      }
    }

    go(numbers, HashSet.empty)
  }

  val list = (0 to 1000000).map(_ => Math.random()).toList
  val value = Math.abs(Math.random())

  val average2 = (0 to 10).map(_ => time(summedHashed(list, value))).sum /11
  val average1 = (0 to 10).map(_ => time(summed(list, value))).sum / 11

  println("\n\n\n\n")
  println(s"***** $average1 vs $average2 ******")
}
