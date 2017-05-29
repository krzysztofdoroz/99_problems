def duplicate[A](list: List[A]) : List[A] = {
  list.foldLeft(List[A]())((acc, el) => el :: el :: acc) reverse
}

duplicate(List())
duplicate(List('a, 2))
duplicate(List('a, 'b, 'c, 'c, 'd))
