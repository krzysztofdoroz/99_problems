def insertAt[A](el : A, n : Int, list: List[A]) : List[A] = {
  val (left, right) = list.splitAt(n)

  left ::: el :: right
}

insertAt('new, 1, List('a, 'b, 'c, 'd))
insertAt('new, 3, List())