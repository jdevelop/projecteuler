import collection.mutable.Map
import collection.mutable.Set

object Euler75 {

  val perimeters: Map[Int, Int] = Map()

  val seen: Set[(Int, Int, Int)] = Set()

  def nod(a: Int, b: Int): Int =
    (a, b) match {
      case (_, 0) => a
      case _ => nod(b, a % b)
    }

  def sort(a: Int, b: Int, c: Int): (Int, Int, Int) = {
    if (a > b) sort(b, a, c)
    else if (b > c) sort(a, c, b)
    else if (a > c) sort(c, b, a)
    else (a, b, c)
  }

  def solve(limOrig: Int) = {
    val lim = math.sqrt(limOrig).toInt
    (for (m <- 1 until lim;
          n <- m + 1 to lim
          if nod(n, m) == 1
    ) yield (n * n - m * m, 2 * m * n, m * m + n * n)).foreach {
      case (a, b, c) => {
        var i = 1
        var z = 0
        do {
          z = (a + b + c) * i
          val zSorted = sort(a * i, b * i, c * i)
          if (z <= limOrig && !seen.contains(zSorted)) {
            perimeters += (z -> (perimeters.getOrElse(z, 0) + 1))
            seen += zSorted
          }
          i = i + 1
        } while (z <= limOrig)
      }
    }
    perimeters.filter {
      case (_, idx) => idx == 1
    }.size
  }

  def main(args: Array[String]) {
    println("Triangles: %1$s".format(solve(args(0).toInt)))
  }
}