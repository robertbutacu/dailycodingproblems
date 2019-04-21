package problems

import scala.annotation.tailrec

object Problem3 extends App {
  /*
  Given a binary array, find the maximum number of consecutive 1s in this array.

  Example 1:
  Input: [1,1,0,1,1,1]
  Output: 3
  Explanation: The first two digits or the last three digits are consecutive 1s.
      The maximum number of consecutive 1s is 3.
  Note:

  The input array will only contain 0 and 1.
  The length of input array is a positive integer and will not exceed 10,000
   */
  def findMaxConsecutiveOnes(nums: Array[Int]): Int = {
    @tailrec
    def go(left: List[Int], currentCount: Int, maxCount: Int): Int = {
      left match {
        case Nil      => Math.max(currentCount, maxCount)
        case h :: tail =>
          if(h == 1) go(tail, currentCount + 1, maxCount)
          else       go(tail, 0, Math.max(currentCount, maxCount))
      }
    }

    go(nums.toList, 0, 0)
  }
}