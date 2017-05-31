import scala.annotation.tailrec

def slice[A](a : Int, b : Int, list: List[A]) : List[A] = {
  if (a < 0 || b < 0 || a > b) return List[A]()

  @tailrec
  def sliceRec[A](a : Int, b : Int, currentIndex : Int, ls : List[A], acc : List[A]) : List[A] = {
    (currentIndex, ls) match {
      case (current, h :: tail) if current < a => sliceRec(a, b, current + 1, tail, acc)
      case (current, h :: tail) if current >= a && current < b => sliceRec(a, b, current + 1, tail, h :: acc)
      case (current, _) if current >= b => acc reverse
      case (_, Nil) => acc reverse
    }
  }

  sliceRec(a, b, 0, list, List[A]())
}

slice(1, 2, List())
slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
