package problems

object Problem4 extends App {
  /**
  Given an array of integers,
  return a new array such that each element at index i of the new array is
  the product of all the numbers in the original array except the one at i.

  For example, if our input was [1, 2, 3, 4, 5], the expected output would be [120, 60, 40, 30, 24].

  If our input was [3, 2, 1], the expected output would be [2, 3, 6].

  Follow-up: what if you can't use division?
  */

  def product(input: List[Int]): List[Int] = {
    val prod = input.product

    input.map(i => prod / i)
  }

  println(product(List(1, 2, 3, 4, 5)))
  println(product(List(3, 2, 1)))

  // for second part, use long-binary division
}
