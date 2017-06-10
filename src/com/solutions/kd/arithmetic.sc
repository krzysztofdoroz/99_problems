import S99Int.int2S99Int

class S99Int(val v: Int) {
  import S99Int._

  def isPrime: Boolean = {
    for (i <- 2 to Math.sqrt(v).toInt) {
      if (v % i == 0) return false
    }
    true
  }

}

object S99Int {
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)
}


7.isPrime
8.isPrime
49.isPrime