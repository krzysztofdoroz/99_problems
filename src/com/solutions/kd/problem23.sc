import scala.annotation.tailrec
import scala.util.Random

def randomSelect[A](n : Int, list: List[A]) : List[A] = {

  if (n > list.size)
    return List()

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

  @tailrec
  def randomSelectRec[A](k : Int, ls : List[A], acc : List[A]) : List[A] = {
    if (k == 0)
      acc
    else {
      val (r, el) = removeAt(Random.nextInt(ls.size), ls)
      randomSelectRec(k - 1, r, el :: acc)
    }
  }

  randomSelectRec(n, list, List())
}

randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
randomSelect(3, List('a, 'b))
