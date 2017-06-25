package collection

object ImageHistogramTree {
  val logger = com.typesafe.scalalogging.Logger(getClass)

  // O(log2(256))
  def addRecursive(data: Int, step: Int): NonEmptyTree =
    if (step == 1) NonEmptyTree(data, NonEmptyTree(data + 1), NonEmptyTree(data - 1), 1)
    else NonEmptyTree(data,
      addRecursive(data + step, step / 2),
      addRecursive(data - step, step / 2),
      1)

  def apply(n: Int = 128): NonEmptyTree = addRecursive(128, 64).+=(0)
}