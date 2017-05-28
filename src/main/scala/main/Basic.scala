package main

import math._

object Basic {
  def assertEquals(x: T, y: T, eps: T) = 
    assert{ abs(x-y) < eps }
  
  type T = Double
  def toDouble(x: Int) = x.toDouble
  def toInt(x: Double) = x.toInt
  def toColorInt(x: Int) = x.toInt.max(0).min(255)
  type B = Boolean
  type BI = java.awt.image.BufferedImage
  
  def deltaX(r: Int, theta: T): Int = (r*cos(theta)).round.toInt
  def deltaY(r: Int, theta: T): Int = (r*sin(theta)).round.toInt
  
  val colorId = 2

  def imgToMInt(img: BI): MInt = 
    if (img.getType != java.awt.image.BufferedImage.TYPE_BYTE_GRAY)
      image.Input.getColorsComponents(img, colorId)
    else image.Input.getMatGrayImage(img)
    
  def imgToM(img: BI): M = mapIT(imgToMInt(img), toDouble(_: Int))
  def intMatToImg(mat: MInt): BI = image.Operation.toImage(mat)
  def intMatToImg(mat: MInt, biType: Int): BI = 
    image.Operation.toImage(mat, biType)
  def doubleMatToImg(mat: M): BI = image.Operation.matrixToImage(mat)
  def matToImg(mat: M, biType: Int): BI = 
    image.Operation.matrixToImage(mat, biType)
  def matToImg(mat: MInt, biType: Int): BI = 
    image.Operation.toImage(mat, biType)
    
  def sqr(a: T): T = a * a
  def sqr(a: Int): Int = a * a
  
  def norm2(x: T, y: T) = sqr(x) + sqr(y)
  def pow4(x: T) = sqr(sqr(x))
  def log2(number: Int): Int = {
    var res = 0
    while ((number >> res) > 0) res +=1
    (res-1)
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
  
  /*
  def sqr2(x: T): T = x*x
  def sqr3[X >: Int with T : reflect.ClassTag](x: Int): X = 
    if (true) sqr1(x)  
    else sqr2(x)
    
  def m[X >: Int with T](x: X): X  = x*x
  */
  /** mat in R^{m,n} => m*n_*/ 
  def productSize(mat: M): Int = mat.length*mat(0).length
  def createM(m: Int, n: Int) = Array.ofDim[T](m, n)
  def createMBool(m: Int, n: Int) = Array.ofDim[B](m, n)
  def print(mat: M) {
    for (str <- mat)
      printAr(str)
    println
  }
  
  def mapTT(mat: M, fun: T => T): M = 
    mat.map{_.map{ x => fun(x) }}
  def mapII(mat: MInt, fun: Int => Int): MInt = 
    mat.map{_.map{ x => fun(x) }}
  def mapTI(mat: M, fun: T => Int): MInt = 
    mat.map{_.map{ x => fun(x) }}
  def mapIT(mat: MInt, fun: Int => T): M = 
    mat.map{_.map{ x => fun(x) }}
  
  def printMat(mat: M) {
    for (str <- mat) printAr(str)
    println
  }
 
  def createMInt(m: Int, n: Int) = Array.ofDim[Int](m, n)
  def createWhiteMat(m: Int, n: Int): M = Array.fill[T](m, n)(255.0)
}