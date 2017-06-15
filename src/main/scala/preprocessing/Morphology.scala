package preprocessing

import other.Types._

object Morphology {
  val logger = com.typesafe.scalalogging.Logger(getClass)

  def erosion(mat: MInt, r: Int): MInt = {
    logger.info(s"erosion matrix started with r=${r}")
    val m = mat.length; val n = mat(0).length
    val res = createMInt(m, n)
    for (y <- r until m - r; x <- r until n - r)
      res(y)(x) =
        {
          var res = 0
          for (i <- -r to r; j <- -r to r)
            res |= mat(y + i)(x + j)
          res
        }
    res
  }

  def dilation(mat: MInt, r: Int): MInt = {
    logger.info(s"dilation matrix started with r=${r}")
    val m = mat.length; val n = mat(0).length
    val res = createMInt(m, n)
    for (y <- r until m - r; x <- r until n - r)
      res(y)(x) =
        {
          var res = 255
          for (i <- -r to r; j <- -r to r)
            res &= mat(y + i)(x + j)
          res
        }
    res
  }

  def closing(mat: MInt, r: Int): MInt = erosion(dilation(mat, r), r)
  def opening(mat: MInt, r: Int): MInt = dilation(erosion(mat, r), r)
  def erosion(img: BI, r: Int): BI =
    intMatToImg(erosion(imgToMInt(img), r))
  def dilation(img: BI, r: Int): BI =
    intMatToImg(dilation(imgToMInt(img), r))
  def closing(img: BI, r: Int): BI =
    intMatToImg(closing(imgToMInt(img), r))
  def opening(img: BI, r: Int): BI =
    intMatToImg(opening(imgToMInt(img), r))
}