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

  def imgToMInt(img: BI): MInt =
    if (img.getType != java.awt.image.BufferedImage.TYPE_BYTE_GRAY)
      image.Operation.getColorsComponents(img, colorId)
    else image.Operation.grayMatFromImage(img)

  def imgToM(img: BI): M = mapIT(imgToMInt(img), toDouble(_: Int))
  def intMatToImg(mat: MInt): BI =
    image.Operation.createTiffImage(mat)
  def doubleMatToImg(mat: M): BI = image.Operation.createTiffImage(mat)

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
  def sum(f: T => T, x1: T, x2: T, h: T) =
    (x1 until x2 by h).map(f).sum
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

  def mapTT(mat: M, fun: T => T): M =
    mat.map { _.map { x => fun(x) } }
  def mapII(mat: MInt, fun: Int => Int): MInt =
    mat.map { _.map { x => fun(x) } }
  def mapTI(mat: M, fun: T => Int): MInt =
    mat.map { _.map { x => fun(x) } }
  def mapIT(mat: MInt, fun: Int => T): M =
    mat.map { _.map { x => fun(x) } }

  def printMat(mat: M) {
    for (str <- mat) printAr(str)
    println
  }

  def createMBool(m: Int, n: Int) = Array.ofDim[Boolean](m, n)
  def createMByte(m: Int, n: Int) = Array.ofDim[Byte](m, n)
  def createMInt(m: Int, n: Int) = Array.ofDim[Int](m, n)
  def createM(m: Int, n: Int) = Array.ofDim[T](m, n)
}