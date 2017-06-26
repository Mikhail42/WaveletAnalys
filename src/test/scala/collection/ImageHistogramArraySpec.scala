package collection

class ImageHistogramArraySpec extends org.scalatest.FlatSpec {
  val logger = com.typesafe.scalalogging.Logger(getClass)

  val r = 25
  val mat = Array.fill[Int](r, r)((math.random() * 255).toInt)
  val H = new Array[Int](256)
  mat.foreach(ar => ar.foreach(x => H(x) += 1))

  val arToAdd = Array.fill[Int](r)((math.random() * 255).toInt)
  val arToRemove = Array.fill[Int](r)((math.random() * 255).toInt)

  val n = 3504 * 2336
  val s = s"add ${n} elements to ImageHistogramArray() and ${n} times calculation of median"

  // it is more faster then use three
  s should "fast" in {
    val hist = ImageHistogramTree()
    val ar = Array.fill[Int](25) { (math.random() * 255).toInt }

    other.Time.time({
      var curMed2 = 2 * 100
      for (i <- 0 until n) {
        for (el <- arToAdd) {
          if (2 * el < curMed2) curMed2 -= 1
          else if (2 * el > curMed2) curMed2 += 1
        }
        for (el <- arToRemove) //TODO: arToRemove)
          if (2 * el < curMed2) curMed2 += 1
          else if (2 * el > curMed2) curMed2 -= 1
      }
    }, s)

    logger.debug(s"hist lenght is ${H.sum}")
  }
}