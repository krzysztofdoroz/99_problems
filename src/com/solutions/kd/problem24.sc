import scala.annotation.tailrec
import scala.util.Random

def lotto(k : Int, n : Int) : List[Int] = {

  @tailrec
  def lottoRec(k : Int, acc : Set[Int]) : List[Int] = {
    if (k == 0)
      acc.toList
    else {
      val el = Random.nextInt(n) + 1
      if (!acc.contains(el)) {
        lottoRec(k - 1, acc + el)
      } else {
        lottoRec(k, acc)
      }
    }
  }

  lottoRec(k, Set())
}

lotto(6, 49)
