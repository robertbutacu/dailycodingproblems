package problems

object Problem8 extends App {
/*
Suppose an arithmetic expression is given as a binary tree. Each leaf is an integer and each internal node is one of '+', '−', '∗', or '/'.

Given the root to such a tree, write a function to evaluate it.

For example, given the following tree:
    *
   / \
  +    +
 / \  / \
3  2  4  5
You should return 45, as it is (3 + 2) * (4 + 5).
 */
  sealed trait Tree[+A, +B]

  case class Leaf[+A, +B](value: A) extends Tree[A, B]
  case class Node[+A, +B](left: Tree[A, B], operation: B, right: Tree[A, B]) extends Tree[A, B]

  sealed trait Operation extends Product with Serializable {
    def compute[F](x: F, y: F)(implicit F: Fractional[F]): F = {
      this match {
        case Addition       => F.plus(x, y)
        case Subtraction    => F.minus(x, y)
        case Division       => F.div(x, y)
        case Multiplication => F.times(x, y)
      }
    }
  }

  case object Addition       extends Operation
  case object Subtraction    extends Operation
  case object Division       extends Operation
  case object Multiplication extends Operation

  trait Compute[T] {
    def combine[F: Fractional](op: T, x: F, y: F): F
  }

  object Compute {
    implicit def operationCompute: Compute[Operation] = new Compute[Operation] {
      override def combine[F: Fractional](op: Operation, x: F, y: F): F = op.compute(x, y)
    }
  }

  def evaluate[F: Fractional, X](tree: Tree[F, X])(implicit C: Compute[X]): F = {
    tree match {
      case Leaf(v)        => v
      case Node(l, op, r) => C.combine(op, evaluate(l), evaluate(r))
    }
  }

  val result = evaluate[Double, Operation](
    Node(
      Node(Leaf(3.0), Addition, Leaf(2.0)),
      Multiplication,
      Node(Leaf(4.0), Addition, Leaf(5.0))): Tree[Double, Operation]
  )

  println(result)
}
