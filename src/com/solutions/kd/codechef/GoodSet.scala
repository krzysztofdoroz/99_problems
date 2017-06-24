import scala.io.StdIn

object Main extends App {
  // your dish goes here

  val N = StdIn.readInt()

  (1 to N).map{ x =>
    val k = StdIn.readInt();
    println(list2String(findGoodSet(k)));
  }

  def findGoodSet(k : Int) : List[Int] = {
    List.range(1, 2*k + 1, 2)
  }

  def list2String(list: List[Int]) : String = {
    list.foldRight("")((x, acc) => x + " " + acc)
  }

}
