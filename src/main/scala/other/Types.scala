package other

import math._

object Types {
  val logger = com.typesafe.scalalogging.Logger(getClass)

  def assertEquals(x: T, y: T, eps: T) =
    assert { abs(x - y) < eps }

  type T = Double
  def toDouble(x: Int) = x.toDouble
  def toInt(x: Double) = x.toInt
  def toColorInt(x: Int) = x.toInt.max(0).min(255)
  type BI = java.awt.image.BufferedImage

  def deltaX(r: Int, theta: Int): Int = (r * other.Constants.coss(theta)).round.toInt
  def deltaY(r: Int, theta: Int): Int = (r * other.Constants.sins(theta)).round.toInt

  val colorId = 2

  def imgToMInt(img: BI): MInt = {
    if (img.getType != java.awt.image.BufferedImage.TYPE_BYTE_GRAY)
      image.Convert.getColorsComponents(img, colorId)
    else image.Convert.grayMatFromImage(img)
  }

  def imgToM(img: BI): M = {
    logger.debug(s"convert image to double matrix")
    mapIT(imgToMInt(img), toDouble(_: Int))
  }
  def intMatToImg(mat: MInt): BI =
    image.Convert.createTiffImage(mat)

  def doubleMatToImg(mat: M): BI = image.Convert.createTiffImage(mat)

  def sqr(a: T): T = a * a
  def sqr(a: Int): Int = a * a

  def norm2(x: T, y: T) = sqr(x) + sqr(y)
  def pow4(x: T) = sqr(sqr(x))
  def log2(number: Int): Int = {
    var res = 0
    while ((number >> res) > 0) res += 1
    (res - 1)
  }

  /** exist k in N_0 : 2^k = x */
  def isBinary(x: Int) = (1 << log2(x)) == x

  type A = Array[Double]
  type ABool = Array[Boolean]
  type AInt = Array[Int]
  type ColectionBI = List[BI]
  import scala.collection.immutable.IndexedSeq
  type ISInt = IndexedSeq[Int]
  def createA(n: Int) = new A(n)
  def createAInt(n: Int) = new AInt(n)
  def printAr(ar: A) {
    for (x <- ar) printf("%3.4f ", x)
    println
  }

  type MT[X] = Array[Array[X]]
  type M = MT[T]
  type MBool = MT[Boolean]
  type MInt = MT[Int]
  type MByte = MT[Byte]

  /** mat in R^{m,n} => m*n_*/
  def productSize(mat: M): Int = mat.length * mat(0).length
  def print(mat: M) {
    for (str <- mat)
      printAr(str)
    println
  }

  def mapTT(mat: M, fun: T => T): M = {
    logger.trace("mapTT started")
    mat.map { _.map { x => fun(x) } }
  }
  def mapII(mat: MInt, fun: Int => Int): MInt = {
    logger.trace("mapII started")
    mat.map { _.map { x => fun(x) } }
  }
  def mapTI(mat: M, fun: T => Int): MInt = {
    logger.trace("mapTI started")
    mat.map { _.map { x => fun(x) } }
  }
  def mapIT(mat: MInt, fun: Int => T): M = {
    logger.trace("mapIT started")
    mat.map { _.map { x => fun(x) } }
  }

  def printMat(mat: M) {
    for (str <- mat) printAr(str)
    println
  }

  def createMBool(m: Int, n: Int) = {
    logger.trace("createMBool started")
    Array.ofDim[Boolean](m, n)
  }
  def createMByte(m: Int, n: Int) = {
    logger.trace("createMByte started")
    Array.ofDim[Byte](m, n)
  }
  def createMInt(m: Int, n: Int) = {
    logger.trace("createMInt started")
    Array.ofDim[Int](m, n)
  }
  def createM(m: Int, n: Int) = {
    logger.trace("createM started")
    Array.ofDim[T](m, n)
  }
}