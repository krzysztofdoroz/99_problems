
def penultimate[A](list : List[A]): Option[A] = {
  list.foldLeft((None, None) : (Option[A], Option[A]))((acc, x : A) => (acc._2, Some(x)))._1
}

penultimate(List())
penultimate(List(1))
penultimate(List(1, 2))
penultimate(List(1, 3, 5))
penultimate(List(1, "a", 2.2))
