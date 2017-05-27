
def reverse[A](l : List[A]) : List[A] = {
  l.foldLeft(List[A]())((acc, el) => el :: acc)
}

reverse(List())
reverse(List(1))
reverse(List(1,2,3))
reverse(List("a","b","c"))
