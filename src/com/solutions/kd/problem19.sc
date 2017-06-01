def rotate[A](n : Int, list: List[A]) : List[A] = {

  def rotateLeft(k : Int, ls : List[A]) : List[A] = {
    val (left, right) = ls.splitAt(k)
    right ::: left
  }

  def rotateRight(k : Int, ls : List[A]) : List[A] = {
    val (left, right) = ls.splitAt(ls.length - k)
    right ::: left
  }

  if (n >= 0) rotateLeft(n, list) else rotateRight(-n, list)
}

rotate(0, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
rotate(3, List())
rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))