package problems

object Problem5 extends App {

  /*
  A unival tree (which stands for "universal value") is a tree where all nodes under it have the same value.

  Given the root to a binary tree, count the number of unival subtrees.

  For example, the following tree has 5 unival subtrees:

   0
  / \
 1   0
    / \
   1   0
  / \
 1   1
   */


  sealed trait Tree[+A]

  case class  Node[+A](value: A,
                       left : Tree[A] = Empty,
                       right: Tree[A] = Empty) extends Tree[A]
  case object Empty                            extends Tree[Nothing]

  def unival[A](tree: Tree[A]): Int = {
    def isSubtreeSame(curr: Tree[A], parent: A): Boolean = {
      curr match {
        case Empty         => true
        case Node(v, l, r) =>
          v == parent && isSubtreeSame(l, parent) && isSubtreeSame(r, parent)
      }
    }

    def go(curr: Tree[A], total: Int): Int = {
      implicit def toInt(boolean: Boolean): Int = if(boolean) 1 else 0

      curr match {
        case Empty         => total
        case Node(v, l, r) => go(l, total) + go(r, total) + (isSubtreeSame(l, v) && isSubtreeSame(r, v))
      }
    }

    go(tree, total = 0)
  }

  val tree = Node(0,
    Node(1),
    Node(0,
      Node(1, Node(1), Node(1)),
      Node(0)))

  println(unival(tree))
}
