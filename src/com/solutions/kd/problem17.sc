import scala.annotation.tailrec

def split[A](n : Int, list: List[A]) : (List[A], List[A]) = {

  @tailrec
  def splitRec(ls : List[A], acc : List[A], k : Int) : (List[A], List[A]) = {
    (ls, k) match {
      case (l, 0) => (acc reverse, l)
      case (h :: tail, _) => splitRec(tail, h :: acc, k - 1)
      case (Nil, _) => (acc reverse, List())
    }
  }

  splitRec(list, List[A](), n)
}

split(3, List())
split(3, List('a, 'b))
split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
split(3, List('a, 'b, 'c))
