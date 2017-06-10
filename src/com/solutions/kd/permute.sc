import scala.annotation.tailrec

def permute[A](list: List[A]): List[List[A]] = {

  def removeEl[A](list: List[A], el: A): List[A] = {
    @tailrec
    def removeElRec[A](ls: List[A], acc: List[A], el: A): List[A] = {
      ls match {
        case h :: tail => if (h == el) acc ::: tail else removeElRec(tail, h :: acc, el)
        case Nil => acc
      }
    }

    removeElRec(list, Nil, el)
  }

  def permuteRec[A](acc: List[A], rest: List[A], result: List[List[A]]): List[List[A]] = {
    if (rest isEmpty)
      return acc :: result
    else {
      val allPerms = for {
        el <- rest
      } yield permuteRec(el :: acc, removeEl(rest, el), result)

      allPerms flatten
    }
  }

  permuteRec(Nil, list, List())
}

permute(List(1, 3, 4))
permute(List(1, 2))
permute(List(1))
