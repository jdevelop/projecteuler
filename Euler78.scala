import collection.mutable

object Euler78 {

  type Args = (BigInt, BigInt)

  type Cache = mutable.Map[Args, BigInt]

  val ZERO = BigInt(0)
  val ONE = BigInt(1)

  def p(k: BigInt, n: BigInt, m: Cache): BigInt = {

    (k, n) match {
      case _ if k > n => 0
      case _ if k == n => 1
      case _ => 
        val z = calc(k + 1, n, m) + calc(k, n - k, m)
        m += (k,n) -> z
        z
    }
  }

  def calc(k: BigInt, n: BigInt, m: Cache) = {
    m.get((n, k)) match {
      case Some(z) => z
      case None => 
        val z = p(k, n, m)
        m += (k,n) -> z
        z
    }
  }

  def solve(num: Int) {
    val m: Cache = mutable.HashMap()
    for (i <- 5000 to 100000) {
      if (i % 1000 == 0) {
        println("Clearing map")
        m.clear()
      } else if (i % 100 == 0) {
        println(i + " => " +m.size)
      } 
      val x = p(i, i, m)
      if (x % num == 0) {
        println("%1$d -> %2$d".format(i, x))
        return
      }
    }
  }

  def main(args: Array[String]) {
    args match {
      case Array(x, _*) => solve(x.toInt)
      case _ => println("Pls provide divisor")
    }
  }

}
