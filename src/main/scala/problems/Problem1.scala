package problems

import scala.annotation.tailrec

/*
There's a staircase with N steps, and you can climb 1 or 2 steps at a time.
Given N, write a function that returns the number of unique ways you can climb the staircase.
The order of the steps matters.

For example, if N is 4, then there are 5 unique ways:

1, 1, 1, 1
2, 1, 1
1, 2, 1
1, 1, 2
2, 2
What if, instead of being able to climb 1 or 2 steps at a time, you could climb any number from a set of positive integers X?
For example, if X = {1, 3, 5}, you could climb 1, 3, or 5 steps at a time. Generalize your function to take in X.
 */
object Problem1 extends App {
  def staircase(n: Int, possibleSteps: List[Int]): List[List[Int]] = {
    def go(remainingN: Int, currentSolution: List[Int], totalSolutions: List[List[Int]]): List[List[Int]] = {
      remainingN match {
        case negative if negative < 0 => totalSolutions
        case 0                        => totalSolutions :+ currentSolution
        case other                    => possibleSteps.foldLeft(totalSolutions){
          (acc, curr) =>
            go(other - curr, currentSolution :+ curr, acc)
        }
      }
    }

    go(n, List.empty, List.empty)
  }

  println(staircase(4, List(1,2)))
}
