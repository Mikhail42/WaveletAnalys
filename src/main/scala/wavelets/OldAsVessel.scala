package wavelets

import math._
import other.Types._

class OldAsVessel(s0: Int = 5) {
  
  def specTransform(img: BI, a: T): (BI, BI) = {
    val res = specTransform(imgToM(img), a)
    (doubleMatToImg(res._1), intMatToImg(res._2))
  }
  
  /** transform with specified norms */
  def specTransform(mat: M, a: T): (M, MInt) = {
    val m = mat.length; val n = mat(0).length 
    val s1 = (s0/a).ceil.toInt
    val s2 = 2
    val s = s1+s2
    val rs = -s to s    
    val dir = createMInt(m, n)
    val res = createM(m, n)
    for (theta <- 0 until 180 by 10) {
      val angle = Pi*theta/180   
      val cs = cos(angle); val sn = sin(angle)
      val xs = rs.map{r => (r*cs).round.toInt}
      val ys = rs.map{r => (r*sn).round.toInt}
      
      for (y <- s until m-s; x <- s until n-s) {
        val g = (-s1 to s1).map{i => mat(ys(i+s)+y)(xs(i+s)+x)}.sum - mat(y)(x) -
          (s1.toDouble/2)*(-s until -s1).map{i => mat(ys(i+s)+y)(xs(i+s)+x)}.sum - 
          (s1.toDouble/2)*(s1+1 to s).map{i => mat(ys(i+s)+y)(xs(i+s)+x)}.sum
        if (g > res(y)(x)) {
          res(y)(x) = g
          dir(y)(x) = theta + 90
        }
      }
    }
    (res, dir)
  }
}