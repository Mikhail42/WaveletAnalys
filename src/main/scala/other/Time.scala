package other

object Time {
  val logger = com.typesafe.scalalogging.Logger(getClass)

  def time[R](block: => R, defin: String): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    logger.info(s"time of ${defin} is ${(t1 - t0) / 1e9} s")
    result
  }
}