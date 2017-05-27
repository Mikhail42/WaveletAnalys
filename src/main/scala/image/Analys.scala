package image

import basic.Basic._
import math._
import java.awt.image._
import basic.Constants._

object Analys {
  val white = 20
  
  def smallFilter(img: BI, amountBorder: Int): BI = {
    val mat = imgToMInt(img)
    val m = mat.length; val n = mat(0).length
    def isWhite(x: Int): Boolean = (x == 255)
    var S = 0
    def analysSmallFilter(i: Int, j: Int, delete: B = false) {
        var centralX = j
        var dy = 0
        while (i+dy >= 0 && isWhite(mat(i+dy)(centralX))) {
          centralX = getNewCentral(dy, centralX) 
          dy -= 1
        }
        dy = 1
        while (i+dy < m && isWhite(mat(i+dy)(centralX))) {
          centralX = getNewCentral(dy, centralX) 
          dy += 1
        }
          
        def getNewCentral(dy: Int, oldCenter: Int): Int = {
          val leftBordX = {
            var dx = 1
            while (j+dx>=0 && isWhite(mat(i+dy)(oldCenter+dx))) {
              if (delete) mat(i+dy)(oldCenter+dx) = 0
              dx -= 1
            }
            j + dx + 1
          }
          val rightBordX = {
            var dx = 1
            while (j+dx < n && isWhite(mat(i+dy)(oldCenter+dx))) {
              if (delete) mat(i+dy)(oldCenter+dx) = 0
              dx += 1
            }
            j + dx - 1
          }
          S += rightBordX-leftBordX
          val centralX = (leftBordX+rightBordX)/2
          centralX
        }
        if (delete)
          if (S < amountBorder)
            analysSmallFilter(i, j, true)
    }
    for (i <- 0 until m; j <- 0 until n)
      if (isWhite(mat(i)(j)))  
        analysSmallFilter(i, j)           
    intMatToImg(mat)
  }
  
  def histogram(img: BI): Array[Int] = {
    val res = new AInt(256)
    val mat = imgToMInt(img)
    val m = mat.length; val n = mat(0).length
    for (i <- 0 until m; j <- 0 until n)
      res(mat(i)(j)) += 1
    res
  }
  
  private def twoMaxHistogramtInds(gi: AInt) = {
    val absMax = gi.max
    val indAbsMax = gi.indexOf(absMax)
    val sum1 = (5 to indAbsMax).map{gi(_)}.sum
    val sum2 = (indAbsMax until 250).map{gi(_)}.sum
    val indLocMax = 
      if (sum1 > sum2) {
        val locMax = (5 to 3*indAbsMax/5).map{gi(_)}.max
        5 + (5 to 3*indAbsMax/5).map{gi(_)}.indexOf(locMax)
      } else {
        val locMax = (indAbsMax+(250-indAbsMax)*2/5 until 250).map{gi(_)}.max
        indAbsMax + (indAbsMax+(250-indAbsMax)*2/5 until 250).map{gi(_)}.indexOf(locMax)
      }
    
    (min(indLocMax, indAbsMax), max(indLocMax, indAbsMax))
  }
  
  private def minHistogramInd(gi: AInt) = {
    val (ind1, ind2) = twoMaxHistogramtInds(gi)
    println(ind1, ind2)
    val mn = (ind1 to ind2).map{gi(_)}.min
    ind1 + (ind1 to ind2).map{gi(_)}.indexOf(mn)
  }
  
  def minHistogram(img: BI): Int = {
    val gi = histogram(img)
    minHistogramInd(gi)
  }
  
  /**search direction matrix
   * use a parallel mapping (outer cycle) 
   * @param mat -- matrix to search direction
   * @param R -- radius for each point, must be non negative integer
   * @param steptheta -- step of theta, must be positive integer
   * @return direction matrix
   */
  def direction(mat: MInt, R: Int = 5, steptheta: Int = 5, extr: String = "ARGMIN"): MInt = {
    val m = mat.length; val n = mat(0).length
    val rs = (-R to R)
    def dys(theta: Int) = rs.map{r => deltaY(r, theta) }
    def dxs(theta: Int) = rs.map{r => deltaX(r, theta) }
    val allDYs = (0 until 180 by steptheta).map{dys(_)}
    val allDXs = (0 until 180 by steptheta).map{dxs(_)}
    
    val res = createMInt(m, n)
    (0 until m).par.map { 
      i =>
        for(j <- 0 until n)
          res(i)(j) = argDisp(i, j)
    }
    
    def getDisp(i: Int, j: Int, theta: Int): T = {
      val ind = theta / steptheta
      val DYs = allDYs(ind); val DXs = allDXs(ind)
      val ys = DYs.map{ dy => (i+dy).max(0).min(m-1) }
      val xs = DXs.map{ dx => (j+dx).max(0).min(n-1) }
      val myx = (0 to 2*R).map{ i => mat(ys(i))(xs(i)) }
      val mx = myx.sum
      val mx2 = myx.map{x => x*x}.sum
      val s = (2*R.toInt+1).toDouble
      (mx2.toDouble/s - (mx*mx).toDouble/(s*s))
    }
    
    def argDisp = 
      if (extr == "ARGMIN") argMinDisp(_, _)
      else argMaxDisp(_, _)
    
    def argMinDisp(i: Int, j: Int): Int = {
      val disps = (0 until 180 by steptheta).map{getDisp(i, j, _)}
      disps.indexOf(disps.min)*steptheta
    }
    
    def argMaxDisp(i: Int, j: Int): Int = {
      val disps = (0 until 180 by steptheta).map{getDisp(i, j, _)}
      disps.indexOf(disps.max)*steptheta
    }
    
    val medRes = createMInt(m, n)
    (1 until m-1).par.map { 
      i =>
        for(j <- 1 until n-1)
          medRes(i)(j) = locMed(i, j)
    }
    
    def locMed(i: Int, j: Int) : Int = {
      var sum = 0
      for (dy <- -1 to 1; dx <- -1 to 1)
        sum += res(i+dy)(j+dx)
      sum / 9
    }
    
    medRes
  }
  
  def mediate(directly: MInt, matToUpdate: MInt) {
    val m = directly.length; val n = directly(0).length
    val mediateMat = createMBool(m, n)
    for (y <- 0 until m; x <- 0 until n)
      if (matToUpdate(y)(x) > white) { 
        val (yMed, xMed): (Int, Int) = 
          Analys.getMediateLine(matToUpdate, x, y, directly(y)(x))
        if (sqr(yMed-y)+sqr(xMed-x) <= 2)
          mediateMat(yMed)(xMed) = true
      }
      
    for (y <- 0 until m; x <- 0 until n) 
      matToUpdate(y)(x) = 
        if (!mediateMat(y)(x)) 0 
        else 255
  }
  
  /** search a image direction */
  def direction(img: BI): BI = intMatToImg(direction(imgToMInt(img)))
  
  /** compare binary images
   * @param img1 -- my image: type == 10, all values is -1 or 0
   * @param img2 -- ideal image: type == 10, all values is -1 or 0
   * @return pair (errorWhite/white, errorBlack/black)
   */
  def compareBinaryImages(img1: BI, img2: BI): (T, T) = {
    val pixs1 = Input.getPixels(img1)
    val pixs2 = Input.getPixels(img2)
    if (pixs1.length != pixs2.length) 
      throw new Exception("Probably, Image Format Exception or Size Image Exception")
    val n = pixs1.length
    var white = 0
    var errorWhite = 0
    var black = 0
    var errorBlack = 0
    val m1: Byte = -1 // probably, it is white
    val m0: Byte = 0  // probably, it is black
    for (i <- 0 until n) {
      if (pixs2(i) == m1) {
        white += 1
        if (pixs1(i) != m1) 
          errorWhite += 1
      } 
      else 
        if (pixs2(i) == m0) {
          black += 1
          if (pixs1(i) != m0)
            errorBlack += 1
        }
    }
    if (white+black != n) throw new Exception("Images to compare must be binary")
    (errorWhite.toDouble / white, errorBlack.toDouble / black)
  }
  
  /**
   * @param imgMat -- components of image, access to element in format (h,w)
   * @param angle -- (directly of blood vessel - pi/2), in [-pi/2, pi/2).
   * @param x -- current value pixel's width
   * @param y -- current value pixel's height 
   * @return (r1, r2), 
   * 	where #r1 -- radius of the motion in the direction of the #angle 
   * 		from the point (#x, #y) until the last black dot in, #r2 -- in the direction of the (#angle+Pi). 
   */
  def radsWhiteLine(imgMat: MInt, theta: Int, x: Int, y: Int): (Int, Int) = {
    val m = imgMat.length; val n = imgMat(0).length
    /** @see image.ImageWaveletInterface.white */
    def isWhite(x: Int, y: Int) = imgMat(y)(x) > white
    def inMatrix(x: Int, y: Int) = (x >= 0 && y >= 0 && x < n && y < m)
    def r(theta: Int): Int = {
      val angle = theta*Pi/180
      val cosA = cos(angle); val sinA =  sin(angle)
      var r = 0
      var kx = x; var ky = y
      while (inMatrix(kx, ky) && isWhite(kx, ky)) {
        kx = x+toInt(r*cosA)
        ky = y+toInt(r*sinA) 
        r += 1
      } 
      r
    }
    
    (r(theta)-1, r(theta+180)-1)
  }
  
  def lengthWhiteLine(imgMat: MInt, theta: Int, x: Int, y: Int): Int = {
    val rads = radsWhiteLine(imgMat, theta, x, y)
    rads._2 + rads._1
  }
  
  /**
   * @param imgMat -- components of image, access to element in format (h,w)
   * @param theta -- directly of line, in [0, 180).
   * @param x -- current value pixel's width
   * @param y -- current value pixel's height
   * @return (yMediate, xMediane) -- coordiane of mediate white line: 
   * 		(theta + 90[degree] = directly(line)) & ((y,x) in line)
   */
  def getMediateLine(imgMat: MInt, x: Int, y: Int, theta: Int): (Int, Int) = {
    val (r1, r2) = radsWhiteLine(imgMat, theta+90, x, y)
    val cosA = coss(theta+90); val sinA =  sins(theta+90)
    
    val xMed = x + toInt((r1-r2)*cosA/2)
    val yMed = y + toInt((r1-r2)*sinA/2)
    
    (yMed, xMed)
  }
}