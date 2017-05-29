
def combinations[A](k : Int, list: List[A]) : List[List[A]] = {

  def comb[A](ls : List[A], acc : List[A], result : List[List[A]], k : Int) : List[List[A]] = {
    (ls ,k) match {
      case (_, 0) => acc :: result
      case (h :: tail, k) => comb(tail, h :: acc, result, k - 1) ::: comb(tail, acc, result, k)
      case (Nil, _) => result
    }
  }

  comb(list, List[A](), List[List[A]](), k)
}

combinations(1, List(1, 2))
combinations(2, List(1, 2, 3))
combinations(3, List('a, 'b, 'c, 'd))
