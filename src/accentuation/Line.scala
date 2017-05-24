package accentuation

import basic.Basic._
import image._
import math._
import basic.Constants._

/** to accentuation line */
object Line {
  
  /**
   * @param img -- image to accentuation line
   * @param r -- inner radius of line-filter
   * @param theta -- angle (in 0 until 180) to line rotate
   * @see #lineSegment(mat, r, theta) 
   * @return image with accentuation line
   */
  def accent(img: BI, theta: Int, r: Int): BI = {
    val resMat: MInt = accent(getMInt(img), theta, r)  
    Operation.toImage(resMat, img.getType)
  }

  /** 
   * @see #lineSegment(img, theta, r)
   * using a #par.map 
   **/
  def accent(mat: MInt, theta: Int, r: Int): MInt = {
    val m = mat.length; val n = mat(0).length
    val inR = r
    val outR = 2*r // outer radius of line-filter
    val core = (-outR until outR).map{i => if (abs(i) < inR) 1 else -1}
    
    def locMask(i: Int, j: Int): Int = {
      var sum: Int = 0
      for (r <- -outR until outR) {
        val dx = getDX(r, theta)
        val dy = getDY(r, theta)
        sum += mat(i+dy)(j+dx)*core(r+outR)
      }
      sum
    }
    
    val res = createMInt(m, n)
    (outR until m-outR).par.map {
      i => 
        for(j <- outR until n-outR)
          res(i)(j) = mat(i)(j)*locMask(i, j)
    }
    
    res
  }
}