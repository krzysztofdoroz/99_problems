
def duplicateN[A](n : Int, list: List[A]) = {
  list.foldLeft(List[A]())((acc, el : A) => List.fill(n)(el) ::: acc) reverse
}

duplicateN(3, List('a, 'b, 'c, 'c, 'd))
duplicateN(3, List())