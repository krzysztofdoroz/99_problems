import S99Int.int2S99Int

import scala.annotation.tailrec

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

  @tailrec
  final def gcd(a : Int, b : Int) : Int = {
    if (a == 0 || b == 0)
      return a + b
    else if (a > b)
      gcd(a % b, b)
    else {
      gcd(a, b % a)
    }
  }
}

7.isPrime
8.isPrime
49.isPrime
S99Int.gcd(36, 63)
