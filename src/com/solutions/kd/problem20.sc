import java.util.NoSuchElementException

import scala.annotation.tailrec

def removeAt[A](n : Int, list: List[A]) : (List[A], A) = {
  if (n < 0 || n > list.length) throw new NoSuchElementException

  @tailrec
  def removeAtRec[A](k : Int, ls : List[A], acc : List[A]) : (List[A], A) = {
    (k, ls) match {
      case (k, h :: tail) if k == n => ((acc reverse) ::: tail, h)
      case (k, h :: tail) if k < n => removeAtRec(k + 1, tail, h :: acc)
      case (_, Nil) => throw new NoSuchElementException
    }
  }

  removeAtRec(0, list, List[A]())
}

removeAt(1, List('a, 'b, 'c, 'd))
removeAt(1, List())