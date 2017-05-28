import scala.annotation.tailrec

def encode[A](list: List[A]) : List[Any] = {
  if (list.isEmpty) return List[(Int, A)]()

  @tailrec
  def encodeRec(ls : List[A], acc : List[Any], current : (Int, A) ) : List[Any] = {
    ls match {
      case h :: tail => if (h == current._2) encodeRec(tail, acc, (current._1 + 1, h))
                        else if (current._1 == 1) encodeRec(tail, current._2 :: acc, (1, h))
                        else encodeRec(tail, current :: acc, (1, h))
      case Nil => if (current._1 == 1) (current._2 :: acc) reverse else (current :: acc) reverse
    }
  }

  encodeRec(list, List[(Int, A)](), (0, list.head))
}

encode(List())
encode(List(12))
encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))