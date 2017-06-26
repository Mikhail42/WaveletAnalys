package collection

import ImageHistogramTree._

class ImageHistogramTreeSpec extends org.scalatest.FlatSpec {
  val logger = com.typesafe.scalalogging.Logger(getClass)

  "addRecursive(2, 1)" should "((3), 2, (1)" in {
    assert(addRecursive(2, 1).equals(NonEmptyTree(2, NonEmptyTree(3), NonEmptyTree(1), 1)))
  }

  "addRecursive(4, 1)" should "((5), 4, (3)" in {
    assert(addRecursive(4, 1).equals(NonEmptyTree(4, NonEmptyTree(5), NonEmptyTree(3), 1)))
  }

  "addRecursive(4, 2)" should "(((7), 6, (5)), 4, ((3), 2, (1)))" in {
    assert(addRecursive(4, 2).equals(NonEmptyTree(4,
      NonEmptyTree(6, NonEmptyTree(7), NonEmptyTree(5), 1),
      NonEmptyTree(2, NonEmptyTree(3), NonEmptyTree(1), 1), 1)))
  }

  val hist: NonEmptyTree = ImageHistogramTree()
  logger.debug(s"ImageHistogramTree() is ${hist}")

  "ImageHistogramTree deep" should "9" in {
    assert(hist.deep == 9)
  }

  "ImageHistogramTree length" should "256" in {
    assert(hist.count == 256)
  }

  val n = 0 //3504 * 2336
  val s = s"add ${n} elements to ImageHistogramTree() and ${n} times calculation of median"

  s should "fast" in {
    val hist = ImageHistogramTree()
    val ar = Array.fill[Int](25) { (math.random() * 255).toInt }

    other.Time.time({
      // O(n*ar.length*4*log2(256)) approximate O(1000*n) approximate about 10^10 operation
      // my PC: 2.4*10^9 Hz, 8 cores (but I can't make parallel algorithm for this task
      // => I can get about 4 seconds -- in better case, theoretically.
      // In real life, I get 12 seconds.

      // for more faster, see ImageHistogramArraySpec (beta version)
      for (i <- 0 until n) {
        hist.++=(ar)
        hist.--=(ar)
        hist.nth(128)
      }
    }, s)

    logger.debug(s"hist lenght is ${hist.count}")
    logger.debug(s"hist deep is ${hist.deep}")
  }
}