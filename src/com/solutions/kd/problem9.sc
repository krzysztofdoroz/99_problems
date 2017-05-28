
def pack[A](l : List[A]) : List[List[A]] = {
  if (l isEmpty) return List[List[A]]()

  def packRec[A](ls : List[A], acc : List[List[A]], tmp : List[A], el : A) : List[List[A]] = {
    ls match {
      case h :: tail => if (h == el) packRec(tail, acc, h :: tmp, h) else packRec(tail, tmp :: acc, List(h), h)
      case Nil => (tmp :: acc) reverse
    }
  }

  packRec(l, List[List[A]](), List[A](), l.head)
}

pack(List('a))
pack(List())
pack(List('a, 2))
pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
