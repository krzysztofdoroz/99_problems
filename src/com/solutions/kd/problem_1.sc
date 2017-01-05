

def last[A](list : List[A]) : Option[A] = {

  def lastElem[A](l : List[A], last : Option[A]) : Option[A] = {
    l match {
      case Nil => last
      case h::tail => lastElem(tail, Some(h))
    }
  }

  lastElem(list, None)
}

def lastUsingFoldLeft[A](list : List[A]) : Option[A] = {

  list.foldLeft(None : Option[A])((acc : Option[A], el : A) => Some(el))

}

last(List(1,2,3))
last(List())
last(List("a"))
last(List("a", "b"))

lastUsingFoldLeft(List(1,2,3))
lastUsingFoldLeft(List())
lastUsingFoldLeft(List("a"))
lastUsingFoldLeft(List("a", "b"))
