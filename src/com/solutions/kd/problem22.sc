import scala.annotation.tailrec

def range(a : Int, b : Int) : List[Int] = {
  if (a > b) return List()

  @tailrec
  def rangeRec(i : Int, j : Int, acc : List[Int]) : List[Int] = {
    (i, j) match {
      case (i, j) if i > j => acc reverse
      case (_, _) => rangeRec(i + 1, j, i :: acc)
    }
  }

  rangeRec(a, b, List())
}

range(4, 9)
range(4, 4)