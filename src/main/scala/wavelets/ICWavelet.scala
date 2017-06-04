package wavelets

import other.Types._
import other.Constants._

abstract class ICWavelet {
  val wavename: String
  val h: T = 0.1
  
  /** 2D wavelet-function */
  def psi(x: T, y: T) : T = ???
  /** int_{x0,y0}^{x0+1, y0+1} psi(x, y) dx dy */
  protected def integral(xBeg: (T, T), xEnd: (T, T)): T = 
    other.Integral.simpson2(psi, xBeg, xEnd, h) 
    
  /** 2D core of wavelet */
  def core(sx: Int, sy: Int, theta: Int, a: T): M = {
    // r(-theta)/a
    val cs = coss(theta)/a // cos -sin 
    val sn = sins(theta)/a // sin  cos
    val res = createM(2*sy+1, 2*sx+1)
    for (y <- -sy to sy; x <- -sx to sx) {
      val newX = ( cs*x + sn*y).round.toInt
      val newY = (-sn*x + cs*y).round.toInt
      res(y+sy)(x+sx) = integral((newX, newY), (newX+1, newY+1))/a
    }
    res
  }
}