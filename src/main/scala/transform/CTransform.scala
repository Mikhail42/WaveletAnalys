package transform

import wavelets._
import other.Basic._
import image._
import image.Operation._
import math._

class CTransform(wavelet: ICWavelet, dx: Int = 4, dy: Int = 4) {

  def transform(mat: M, theta: Int, a: T): M = {
    val sx = (dx / a).floor.toInt
    val sy = (dy / a).floor.toInt
    val core: M = wavelet.core(sx, sy, theta, a)
    val coef: T = core.map { _.sum }.sum
    // assert{abs(coef) > 0.01 && abs(coef) < 100}
    val m = mat.length; val n = mat(0).length
    val res = createM(m, n)
    for (y <- sy until m - sy; x <- sx until n - sx) {
      var sum: T = 0
      for (k <- -sy to sy; l <- -sx to sx)
        sum += mat(k + y)(l + x) * core(k + sy)(l + sx)
      res(y)(x) = sum / coef
    }
    res
  }

  def WT(img: BI, theta: Int, a: T): M =
    transform(
      mapIT(getColorsComponents(img, 2), (x: Int) => x.toDouble),
      theta,
      a)
}