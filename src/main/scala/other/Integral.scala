package other
import other.Types._

object Integral {
  val logger = com.typesafe.scalalogging.Logger(getClass)

  /** return min n: n = 2*N+1 & (x2-x1)/h0 <= n */
  def getN(x1: T, x2: T, h0: T): Int =
    ((x2 - x1) / h0).ceil.toInt | 1

  /** @see "Simpsons rule" in Internet */
  def simpson(f: T => T, x1: T, x2: T, h0: T): T = {
    val n = getN(x1, x2, h0)
    val h = (x2 - x1) / n
    val s1 = sum(f, x1 + h, x2 - h, 2 * h)
    val s2 = sum(f, x1 + 2 * h, x2 - h, 2 * h)
    h * (2 * s1 + 4 * s2 + f(x1) + f(x2)) / 3
  }

  /** Simpsons rule for 2D
   *  @see simpson
   */
  def simpson2(f: (T, T) => T, xBeg: (T, T), xEnd: (T, T), h0: T): T = {
    logger.debug(s"simpson2 started with step ${h0} fro ${xBeg} to ${xEnd}")

    val n1 = getN(xBeg._1, xEnd._1, h0)
    val h1 = (xEnd._1 - xBeg._1) / n1
    val n2 = getN(xBeg._2, xEnd._2, h0)
    val h2 = (xEnd._2 - xBeg._2) / n2
    def locSimpson(g: T => T): T = simpson(g, xBeg._2, xEnd._2, h2)
    def sum(f: (T, T) => T, x1: T, x2: T, h: T): T = {
      var sum: T = 0
      for (x <- x1 until x2 by h)
        sum += locSimpson(f(x, _))
      sum
    }
    val s1: T = sum(f, xBeg._1 + h1, xEnd._1, 2 * h1)
    val s2: T = sum(f, xBeg._1 + 2 * h1, xEnd._1, 2 * h1)
    (h1 / 3) * (4 * s1 + 2 * s2 + locSimpson(f(xBeg._1, _)) + locSimpson(f(xEnd._1, _)))
  }
}