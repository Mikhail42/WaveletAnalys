package collection

class BinaryTreeSpec extends org.scalatest.FlatSpec {
  val logger = com.typesafe.scalalogging.Logger(getClass)
  val hist: NonEmptyTree = ImageHistogramTree()

  logger.debug(s"ImageHistogramTree() is ${hist}")

  "addRecursive(2, 1)" should "((3), 2, (1)" in {
    assert(ImageHistogramTree.addRecursive(2, 1).equals(NonEmptyTree(2, NonEmptyTree(3), NonEmptyTree(1), 1)))
  }

  "addRecursive(4, 1)" should "((5), 4, (3)" in {
    assert(ImageHistogramTree.addRecursive(4, 1).equals(NonEmptyTree(4, NonEmptyTree(5), NonEmptyTree(3), 1)))
  }

  "addRecursive(4, 2)" should "(((7), 6, (5)), 4, ((3), 2, (1)))" in {
    assert(ImageHistogramTree.addRecursive(4, 2).equals(NonEmptyTree(4,
      NonEmptyTree(6, NonEmptyTree(7), NonEmptyTree(5), 1),
      NonEmptyTree(2, NonEmptyTree(3), NonEmptyTree(1), 1), 1)))
  }

  "ImageHistogramTree().deep" should "9" in {
    assert(hist.deep == 9)
  }

  "ImageHistogramTree().length" should "256" in {
    assert(hist.count == 256)
  }

  "(., 128, .) += 64" should "(., 128, (., 64, .))" in {
    assert(EmptyTree.equals(EmptyTree))
    assert(NonEmptyTree(128).+=(64).equals(NonEmptyTree(128, EmptyTree, NonEmptyTree(64), 1)))
    assert(NonEmptyTree(128).+=(64).+=(96)
      .equals(
        NonEmptyTree(128,
          EmptyTree, NonEmptyTree(64,
            NonEmptyTree(96), EmptyTree, 1), 1)))
  }

  "NonEmptyTree(128).+=(64).+=(96).+=(1))" should "(., 128, ((., 96, .), 64, (., 1, .)))" in {
    NonEmptyTree(128).+=(64).+=(96).+=(1).toString == "(., 128, ((., 96, .), 64, (., 1, .)))"
  }

  val n = 3504 * 2336
  val s = s"add ${n} elements to ImageHistogramTree() and ${n} times calculation of median"

  s should "fast" in {
    val hist = ImageHistogramTree()
    val ar = Array.fill[Int](25) { (math.random() * 255).toInt }

    other.Time.time({
      // O(n*ar.length*4*log2(256)) approximate O(1000*n) approximate about 10^10 operation
      // my PC: 2.4*10^9 Hz, 8 cores (but I can't make parallel algorithm for this task
      // => I can get about 4 seconds -- in better case, theoretically.
      // In real life, I get 12 seconds.

      // It is fast algorithm: binary three allow make any operation (search mean, add or remove) at O(log2(256)) operation
      // If use array, you get n*(4*r + (sort of 3*r^2)) = O(n*3*r^2*log2(3*r^2)) = O(20000*n) = O(2*10^11) operations --
      // about in 20 times more. And it is use fast algorithm O(r), but not O(r^2), that commonly used students

      // Exists algorithm at n*O(1) operations, but O(1) as O(256*(3 + log2(256)), so you will need union and difference three histograms
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