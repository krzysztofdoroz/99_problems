
def compress[A](l : List[A]) : List[A] = {
  if (l.isEmpty) return l

  l.foldLeft((List[A](l.head), l.head))((a, el) => if (a._2 == el) (a._1, a._2) else (el :: a._1, el))._1 reverse
}

compress(List('a))
compress(List())
compress(List('a, 2))
compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
