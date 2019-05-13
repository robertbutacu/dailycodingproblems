package problems


import scala.annotation.tailrec
import scala.collection.immutable.HashSet

/*
Given a list of numbers and a number k, return whether any two numbers from the list add up to k.

For example, given [10, 15, 3, 7] and k of 17, return true since 10 + 7 is 17.

Bonus: Can you do this in one pass?
 */

/**
  Simply by changing the implementation from a List-based one to a HashSet based one,
  we can see a performance increase of close to 120 times.
  Edit: we can see that prepending to the list instead of appending 4x increased performance.
  */

object Problem2 extends App with TimingHelpers {
  type Result = (Double, Double)

  def summedPrepending(numbers: List[Double], k: Double): Option[Result] = {
    @tailrec def go(remaining: List[Double], generated: List[Double]): Option[Result] = {
      remaining match {
        case Nil       => None
        case h :: tail =>
          if(generated.contains(h)) Option((k - h, h))
          else                      go(tail, (k - h) +: generated)
      }
    }

    go(numbers, List.empty)
  }

  def summedAppending(numbers: List[Double], k: Double): Option[Result] = {
    @tailrec def go(remaining: List[Double], generated: List[Double]): Option[Result] = {
      remaining match {
        case Nil       => None
        case h :: tail =>
          if(generated.contains(h)) Option((k - h, h))
          else                      go(tail, generated :+ (k - h))
      }
    }

    go(numbers, List.empty)
  }

  def summedHashed(numbers: List[Double], k: Double): Option[Result] = {
    @tailrec def go(left: List[Double], hashed: Set[Double]): Option[Result] = {
      left match {
        case Nil       => None
        case h :: tail =>
          if(hashed.contains(h)) Option((h, k - h))
          else                   go(tail, hashed + (k - h))
      }
    }

    go(numbers, HashSet.empty)
  }

  val list = (0 to 10000).map(_ => Math.random()).toList
  val value = Math.abs(Math.random())

  val average2 = (0 to 10).map(_ => time(summedHashed(list, value))).sum / 11
  val average1 = (0 to 10).map(_ => time(summedPrepending(list, value))).sum / 11
  val average3 = (0 to 10).map(_ => time(summedAppending(list, value))).sum / 11

  println("\n\n\n\n")
  println(s"***** Prepend: $average1 vs Hashed: $average2 vs Append: $average3******")
}
