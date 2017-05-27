package preprocessing

import basic.Basic._

object DilateErose {
  val r = 1
  def erose(mat: MInt): MInt = {
    val m = mat.length; val n = mat(0).length
    val res = createMInt(m, n)
    for (y <- r until m-r; x <- r until n-r)
      res(y)(x) = 
      {
        var res = 0
        for (i <- -r to r; j <- -r to r)
          res |= mat(y+i)(x+j)
        res
      }
    res
  }
  
  def dilate(mat: MInt): MInt = {
    val m = mat.length; val n = mat(0).length
    val res = createMInt(m, n)
    for (y <- r until m-r; x <- r until n-r)
      res(y)(x) = 
      {
        var res = 255
        for (i <- -r to r; j <- -r to r)
          res &= mat(y+i)(x+j)
        res
      }
    res
  }
  
  def eroseDilete(mat: MInt): MInt = erose(dilate(mat))
  def dilateErose(mat: MInt): MInt = dilate(erose(mat))
  def erose(img: BI): BI = 
    intMatToImg(erose(imgToMInt(img)))
  def dilate(img: BI): BI = 
    intMatToImg(dilate(imgToMInt(img)))
  def eroseDilete(img: BI): BI = 
    intMatToImg(eroseDilete(imgToMInt(img)))
  def dilateErose(img: BI): BI = 
    intMatToImg(dilateErose(imgToMInt(img)))
}