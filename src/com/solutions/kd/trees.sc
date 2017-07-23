import scala.annotation.tailrec

sealed abstract class Tree[+T] {
  def isMirrorOf[T](t : Tree[T]) : Boolean
  def isSymmetric : Boolean
  def addValue[U >: T <% Ordered[U]](v : U) : Tree[U]
  def leafCount : Int
  def leafList : List[T]
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

  override def addValue[U >: T <% Ordered[U]](v: U): Tree[U] = {
    if (v > value) {
      Node(value, left, right.addValue(v))
    } else {
      Node(value, left.addValue(v), right)
    }
  }

  override def leafCount : Int = {
    (left, right) match {
      case (End, End) => 1
      case _ => left.leafCount + right.leafCount
    }
  }

  override def leafList: List[T] = {
    (left, right) match {
      case (End, End) => List(value)
      case _ => left.leafList ++ right.leafList
    }
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

  override def addValue[U <% Ordered[U]](value: U): Tree[U] = {
    Node(value)
  }

  override def leafCount : Int = 0

  override def leafList: List[Nothing] = Nil

}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

object Tree {
  def fromList[T <% Ordered[T]](list : List[T]) : Tree[T] = {
    list.foldLeft(End : Tree[T])((acc, v) => acc.addValue(v))
  }

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

End.addValue(2).addValue(3).addValue(4)

Tree.fromList(List(3, 2, 5, 7, 1))

Node('x', Node('x'), End).leafCount
Node('a', Node('b'), Node('c', Node('d'), Node('e'))).leafList