import scala.annotation.tailrec

@tailrec
def nth[A](n : Int, list : List[A]) : Option[A] = {
  (n, list) match {
    case (0, h::tail) => Some(h)
    case (n, h::tail) => nth(n-1, tail)
    case _ => None
  }
}

nth(1, List())
nth(0, List())
nth(1, List(2, 3))
nth(0, List(1))
nth(2, List(1, 2, 3))
nth(3, List())

