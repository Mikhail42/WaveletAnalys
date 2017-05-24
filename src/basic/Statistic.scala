package basic

import math._
import basic.Basic._

object Statistic {
  
  def minMaxAverage(mat: M): (T, T, T) = {
    var min: T = mat(0)(0)
    var max: T = min
    var averange: T = 0
    for (i <- 0 until mat.length; j <- 0 until mat(0).length){
      val x = mat(i)(j)
      if (x < min) min = x
      else 
        if (x > max) max = x
      averange += x
    }
    (min, max, averange/productSize(mat))
  }
  
  def disp(mat: M, aver: T): T = 
    mat.map { _.map { y => sqr(y-aver) }.sum }.sum/
        productSize(mat)
  /** X => EX */
  def aver(mat: M): T = mat.map {_.sum}.sum / productSize(mat)
  
  /** (X,Y) => E[X.*Y] - EX*EY 
   *  @see tests.MathTest#MathToolKitTests
   **/
  def correlation(mat1: M, mat2: M): T = {
    val aver1 = aver(mat1); val aver2 = aver(mat2)
    var sum: T = 0
    for (i <- 0 until mat1.length; j <- 0 until mat1(0).length)
      sum += mat1(i)(j)*mat2(i)(j)
    sum / productSize(mat1) - aver1*aver2
  }
  
  def mxMat(mat: M, sy: Int, sx: Int): M = {
    val m = mat.length; val n = mat(0).length 
    val sumStr = createM(m, n)
    val dxs = (-sx to sx)
    for (y <- sy until m-sy){
      sumStr(y)(sx) = dxs.map{dx => mat(y)(sx+ dx)}.sum
      for (x <- sx+1 until n-sx)
        sumStr(y)(x) = sumStr(y)(x-1) + (mat(y)(x+sx) - mat(y)(x-sx-1))
    }
    
    val sumCol = createM(m, n)
    val dys = (-sy to sy)
    for (x <- sx until n-sx){
      sumCol(sy)(x) = dys.map{dy => sumStr(sy+dy)(x)}.sum
      for (y <- sy+1 until m-sy) {
        val dif = sumStr(y+sy)(x) - sumStr(y-sy-1)(x)
        sumCol(y)(x) = sumCol(y-1)(x) + dif
      }
    }
    
    val S: T = (2*sx+1)*(2*sy+1)
    val mx = sumCol.map{_.map{_/S}}
    mx
  }
  
  def mxMat(mat: MInt, sy: Int, sx: Int): MInt = {
    val m = mat.length; val n = mat(0).length 
    val sumStr = createMInt(m, n)
    val dxs = (-sx to sx)
    for (y <- sy until m-sy){
      sumStr(y)(sx) = dxs.map{dx => mat(y)(sx+ dx)}.sum
      for (x <- sx+1 until n-sx)
        sumStr(y)(x) = sumStr(y)(x-1) + (mat(y)(x+sx) - mat(y)(x-sx-1))
    }
    
    val sumCol = createMInt(m, n)
    val dys = (-sy to sy)
    for (x <- sx until n-sx){
      sumCol(sy)(x) = dys.map{dy => sumStr(sy+dy)(x)}.sum
      for (y <- sy+1 until m-sy) 
        sumCol(y)(x) = sumCol(y-1)(x) + sumStr(y+sy)(x) - sumStr(y-sy-1)(x)
    }
    
    val S = (2*sx+1)*(2*sy+1)
    val mx = sumCol.map{_.map{_/S}}
    mx
  }
  
  def mx2Mat(mat: M, sy: Int, sx: Int): M = {
    val m = mat.length; val n = mat(0).length
    val sumStr = createM(m, n)
    val dxs = (-sx to sx)
    for (y <- sy until m-sy){
      sumStr(y)(sx) = dxs.map{dx => sqr(mat(y)(sx+ dx))}.sum
      for (x <- sx+1 until n-sx)
        sumStr(y)(x) = sumStr(y)(x-1) + sqr(mat(y)(x+sx)) - sqr(mat(y)(x-sx-1))
    }
    val sumCol = createM(m, n)
    val dys = (-sy to sy)
    for (x <- sx until n-sx){
      sumCol(sy)(x) = dys.map{dy => sumStr(sy+dy)(x)}.sum
      for (y <- sy+1 until m-sy)
        sumCol(y)(x) = sumCol(y-1)(x) + sumStr(y+sy)(x) - sumStr(y-sy-1)(x)
    }
    val S: T = (2*sx+1)*(2*sy+1)
    val mx2 = sumCol.map{_.map{_/S}}
    mx2
  }
  
  def mx2Mat(mat: MInt, sy: Int, sx: Int): MInt = {
    val m = mat.length; val n = mat(0).length 
    val sumStr = createMInt(m, n)
    val dxs = (-sx to sx)
    for (y <- sy until m-sy){
      sumStr(y)(sx) = dxs.map{dx => sqr(mat(y)(sx+ dx))}.sum
      for (x <- sx+1 until n-sx)
        sumStr(y)(x) = sumStr(y)(x-1) + sqr(mat(y)(x+sx)) - sqr(mat(y)(x-sx-1))
    }
    val sumCol = createMInt(m, n)
    val dys = (-sy to sy)
    for (x <- sx until n-sx){
      sumCol(sy)(x) = dys.map{dy => sumStr(sy+dy)(x)}.sum
      for (y <- sy+1 until m-sy)
        sumCol(y)(x) = sumCol(y-1)(x) + sumStr(y+sy)(x) - sumStr(y-sy-1)(x)
    }
    val S = (2*sx+1)*(2*sy+1)
    val mx2 = sumCol.map{_.map{_/S}}
    mx2
  }
  
  def dispMat(mat: M, sy: Int, sx: Int): M = {
    val mx = mxMat(mat, sy, sx)
    val mx2 = mx2Mat(mat, sy, sx)
    val m = mat.length; val n = mat(0).length
    val res = createM(m, n)
    for (i <- 0 until m; j <- 0 until n)
      res(i)(j) = mx2(i)(j) - sqr(mx(i)(j))
    res
  }
  
  def dispMat(mat: MInt, sy: Int, sx: Int): MInt = {
    val mx = mxMat(mat, sy, sx)
    val mx2 = mx2Mat(mat, sy, sx)
    val m = mat.length; val n = mat(0).length
    val res = createMInt(m, n)
    for (i <- 0 until m; j <- 0 until n)
      res(i)(j) = mx2(i)(j) - sqr(mx(i)(j))
    res
  }
}