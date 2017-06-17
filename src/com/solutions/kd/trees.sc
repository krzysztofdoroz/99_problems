import scala.annotation.tailrec

sealed abstract class Tree[+T] {
  def isMirrorOf[T](t : Tree[T]) : Boolean
  def isSymmetric : Boolean
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"

  override def isMirrorOf[T](t : Tree[T]) : Boolean = {
    t match {
      case Node(v, l, r) if v != value => false
      case Node(v, l ,r) => left.isMirrorOf(l) && right.isMirrorOf(r)
      case _ => false
    }
  }

  override def isSymmetric : Boolean = {
    left.isMirrorOf(right)
  }
}

case object End extends Tree[Nothing] {
  override def toString = "."

  override def isMirrorOf[T](t: Tree[T]): Boolean = {
    t == End
  }

  override def isSymmetric: Boolean = {
    true
  }
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

object Tree {
//  def cBalanced[A](nodes : Int, s : A) : List[Tree[A]] = {
//
//    @tailrec
//    def cBalancedRec(n : Int) : List[Tree[A]] = {
//      n match {
//        case 1 => Node(s)
//      }
//    }
//
//  }
}

Node(2).isMirrorOf(Node(3))
Node(2).isMirrorOf(Node(2))
Node(2, End, Node(3)).isMirrorOf(Node(2))

Node('a', Node('c'), Node('c')).isSymmetric
