package problems

import scala.annotation.tailrec

/*
Given a list of numbers and a number k, return whether any two numbers from the list add up to k.

For example, given [10, 15, 3, 7] and k of 17, return true since 10 + 7 is 17.

Bonus: Can you do this in one pass?
 */

object Problem2 extends App {
  def summed(numbers: List[Int], k: Int): Boolean = {
    @tailrec
    def go(remaining: List[Int], generated: List[Int]): Boolean = {
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

  println(summed(List(10, 15, 3, 7), 17))
}
