def isPalindrome[A](l : List[A]) : Boolean = {

  def reverse[A](ls : List[A]) : List[A] = {
    ls.foldLeft(List[A]())((acc, el) => el :: acc)
  }

  reverse(l) == l
}

isPalindrome(List())
isPalindrome(List(1))
isPalindrome(List(1,2))
isPalindrome(List(1,2,3,2,1))
isPalindrome(List(1,2,"aa",2,1))
isPalindrome(List(1,2,3,2,2))
