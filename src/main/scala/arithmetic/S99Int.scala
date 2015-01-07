package arithmetic

import scala.math.pow
class S99Int(val start: Int) {
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

  def isPrime: Boolean =
    (start > 1) && (S99Int.primes takeWhile {
      _ <= Math.sqrt(start)
    } forall {
      start % _ != 0
    })

  def gcd(a: Int, b: Int) = {
    if (a == 0 || b == 0) {
      0
    }
    def rec(m: Int, n: Int): Int = {
      val amari = m % n
      if (amari == 0) {
        n
      } else {
        rec(n, amari)
      }
    }
    if (a >= b) {
      rec(a, b)
    } else {
      rec(b, a)
    }
  }

  def isComprimeTo(i: Int): Boolean = gcd(start, i) == 1

  def totient: Int = Stream.cons(1, Stream.from(2, 1)).takeWhile(_ <= start).count(gcd(start, _) == 1)


  def primeFactors: List[Int] = {
    def rec(a: Int, ret: List[Int]): List[Int] = {
      for (i <- S99Int.primes.takeWhile(_ <= a)) {
        if (a % i == 0) {
          return rec(a / i, i :: ret)
        }
      }
      ret
    }
    rec(start, List()).reverse
  }

  // cheat
  def primeFactorMultiplicity: Map[Int, Int] = {
    def factorCount(n: Int, p: Int): (Int, Int) =
      if (n % p != 0) (0, n)
      else factorCount(n / p, p) match {
        case (c, d) => (c + 1, d)
      }
    def factorsR(n: Int, ps: Stream[Int]): Map[Int, Int] =
      if (n == 1) Map()
      else if (n.isPrime) Map(n -> 1)
      else {
        val nps = ps.dropWhile(n % _ != 0)
        val (count, dividend) = factorCount(n, nps.head)
        Map(nps.head -> count) ++ factorsR(dividend, nps.tail)
      }
    factorsR(start, S99Int.primes)
  }

  def tointImprove: Int = {
    val m = start.primeFactorMultiplicity
    def rec(n: List[(Int, Int)],ret:Int): Int = {
      n match {
        case h::t => rec(t,((h._1 -1) *pow( h._1,h._2-1)*ret).toInt)
        case Nil => ret
      }
    }
    rec(m.toList,1)
  }

}

object S99Int {
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

  val primes = Stream.cons(2, Stream.from(3, 2) filter {
    _.isPrime
  })
}