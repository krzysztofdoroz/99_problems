
def flatten(l : List[Any]) : List[Any] = {

  def flat(acc : List[Any], ls : List[Any]) : List[Any] = {
    ls match {
      case (s : List[Any]) :: tail => flat(flat(List(), s) ::: acc, tail)
      case x :: tail => flat(x :: acc, tail)
      case Nil => acc
    }
  }

  flat(List(), l) reverse
}

flatten(List())
flatten(List(1,2))
flatten(List(1, List(2,3)))
flatten(List(1, List(2,3),8))
flatten(List(List(1, 1), 2, List(3, List(5, 8))))
