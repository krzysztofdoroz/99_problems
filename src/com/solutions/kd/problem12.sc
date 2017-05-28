import scala.annotation.tailrec

def decode[A](list: List[(Int, A)]) : List[A] = {

  @tailrec
  def decodeRec(ls : List[(Int, A)], acc : List[A]) : List[A] = {
    ls match {
      case h :: tail => decodeRec(tail, List.fill(h._1)(h._2) ::: acc)
      case Nil => acc reverse
    }
  }

  decodeRec(list, List[A]())
}

decode(List())
decode(List((2, 's)))
decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
