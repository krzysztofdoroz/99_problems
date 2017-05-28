import scala.annotation.tailrec

def encode[A](list: List[A]) : List[(Int, A)] = {
  if (list.isEmpty) return List[(Int, A)]()

  @tailrec
  def encodeRec(ls : List[A], acc : List[(Int, A)], current : (Int, A) ) : List[(Int, A)] = {
    ls match {
      case h :: tail => if (h == current._2) encodeRec(tail, acc, (current._1 + 1, h)) else encodeRec(tail, current :: acc, (1, h))
      case Nil => (current :: acc) reverse
    }
  }

  encodeRec(list, List[(Int, A)](), (0, list.head))
}

encode(List())
encode(List(12))
encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))