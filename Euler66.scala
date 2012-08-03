object Euler66 {

  // a, d, m
  type Triple = Tuple3[Int, Double, Double]

  // p, q
  type Ratio = Tuple2[BigInt, BigInt]

  def chained(x: Int): Stream[Triple] = {
    val a0 = math.floor(math.sqrt(x)).toInt
    val d0 = 1d
    val m0 = 0d
    def loop(v: Triple): Stream[Triple] = {
      val mn = v._2 * v._1 - v._3
      val dn = (x - mn * mn) / v._2
      val an = math.floor((a0 + mn) / dn)
      v #:: loop((an.toInt, dn, mn))
    }
    loop((a0, d0, m0))
  }

  def suitable(x: Int) = {
    val z = chained(x)
    def loop(pm1: Ratio, p0: Ratio, x: Stream[Triple]): Stream[Ratio] = {
      val c = x.head
      val pn = c._1 * p0._1 + pm1._1
      val qn = c._1 * p0._2 + pm1._2
      (pn, qn) #:: loop(p0, (pn, qn), x.drop(1))
    }
    val a = (BigInt(1), BigInt(0))
    val b = (BigInt(z.head._1), BigInt(1l))

    a #:: b #:: loop(a, b, z.drop(1))
  }

  def findRoot(d: Int) = {
    val quotations = suitable(d).drop(1) // not interested in obvious
    def check(z: Ratio) = z._1 * z._1 - d * z._2 * z._2 != 1
    quotations.dropWhile(check).head
  }

  def solve(lim: Int) = {
    def notSquare(x: Int) = {
      val sqrt: Int = math.floor(math.sqrt(x)).toInt
      sqrt * sqrt != x
    }
    (2 to lim).filter(notSquare).map(x => (x, findRoot(x))).sortBy(_._2._1).last._1
  }

}