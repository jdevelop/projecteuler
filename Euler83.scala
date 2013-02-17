import java.io.FileInputStream
import java.util
import util.Comparator

object Euler83 {

  import java.io.InputStream

  type Matrix = Array[Array[Int]]

  def readMatrix(is: InputStream): Matrix =
    io.Source.fromInputStream(is).getLines().map(_.split("[\\s,]+").map(_.toInt)).toArray

  def fillPaths(matrix: Matrix): Matrix = {
    case class Crd(x: Int, y: Int, weight: Int)

    import util.{TreeSet => UTreeSet}

    val queue = new UTreeSet[Crd](new Comparator[Crd] {
      def compare(left: Crd, right: Crd): Int = {
        if (left eq right) {
          0
        } else if (left.weight < right.weight) {
          -1
        } else if (left.weight > right.weight) {
          1
        } else if (left.x < right.x) {
          -1
        } else if (left.x > right.x) {
          1
        } else if (left.y < right.y) {
          -1
        } else if (left.y > right.y) {
          1
        } else {
          0
        }
      }
    })
    val nodes = new Array[Array[(Crd, Boolean)]](matrix.length)
    for (j <- 0 until matrix.length;
         i <- 0 until matrix(j).length;
         if (j > 0 || i > 0)) {
      val crd = Crd(i, j, Int.MaxValue)
      if (nodes(j) == null) {
        nodes(j) = new Array[(Crd, Boolean)](matrix(j).length)
      }
      queue.add(crd)
      nodes(j)(i) = (crd, false)
    }
    val init = Crd(0, 0, matrix(0)(0))
    queue.add(init)
    nodes(0)(0) = (init, true)

    def getItem(x: Int, y: Int): Option[(Crd, Boolean)] = {
      if (y < 0 || y >= matrix.length || x < 0 || x >= matrix(y).length) {
        None
      } else {
        Some(nodes(y)(x))
      }
    }

    val abroad = Array((-1, 0), (0, -1), (1, 0), (0, 1))
    while (!queue.isEmpty) {
      val current = queue.first()
      abroad.map {
        case (dx, dy) => getItem(current.x + dx, current.y + dy)
      }.filter(_.isDefined).filter(!_.get._2).map(_.get).foreach {
        case (crd, _) =>
          val dst = current.weight + matrix(crd.y)(crd.x)
          if (dst < crd.weight) {
            assert(queue.remove(crd), "Can not remove %1$s from %2$s".format(crd, queue))
            val copy: Crd = crd.copy(weight = dst)
            nodes(crd.y)(crd.x) = (copy, false)
            queue.add(copy)
          }
      }
      queue.remove(current)
      nodes(current.y)(current.x) = (current, true)
    }
    nodes.map(_.map(_._1.weight))
  }

  def main(args: Array[String]) {
    val matrix: Matrix = readMatrix(new FileInputStream("matrix.txt"))
    val result = fillPaths(matrix)
    println(result.last.last)
  }

}