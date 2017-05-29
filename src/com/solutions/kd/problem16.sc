import scala.annotation.tailrec

def drop[A](n : Int, list: List[A]) : List[A] = {

  @tailrec
  def dropRec(ls : List[A], acc : List[A], index : Int) : List[A] = {
    ls match {
      case h :: tail => if (index % n == 0) dropRec(tail, acc, index + 1) else dropRec(tail, h :: acc, index + 1)
      case Nil => acc reverse
    }
  }

  dropRec(list, List[A](), 1)
}

drop(3, List())
drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
