package tests

import math._

import other.Types._
import transform.Transform._
import image.Output._
import image.Convert._
import Base._

object DWTTest {
  val logger = com.typesafe.scalalogging.Logger(getClass)

  def assertEquals(x: T, y: T, eps: T) =
    assert { abs(x - y) < eps }

  def dwtTests {
    Daubechies1DTest
    Daubechies2DTest
    DaubechiesFullImageTest
    DaubechiesForwardImageTest
  }

  def Daubechies1DTest {
    val n = 250
    val ar: A = new A(n)
    for (i <- 0 until n) ar(i) = i
    for (order <- 1 to 4) {
      val db = new wavelets.Daubechies(order)
      val aed = new transform.AncientEgyptianDecomposition(db)
      val forw = aed.forward1D(ar)
      val revr = aed.reverse1D(forw)
      val dif = (0 until ar.length).map { i => abs(ar(i) - revr(i)) }.sum
      assertEquals(dif, 0.0, 1e-10)
    }
  }

  def Daubechies2DTest {
    val mat: M = Array(
      Array(1, 2, 3, 20, 25),
      Array(4, 5, 6, 20, 26),
      Array(7, 8, 9, 20, 27),
      Array(10, 11, 12, 20, 28))
    val order = 2
    for (order <- 1 to 4) {
      val db = new wavelets.Daubechies(order)
      val aed = new transform.AncientEgyptianDecomposition(db)
      val forw = aed.forward2D(mat)
      val revr = aed.reverse2D(forw)

      val cor = postprocessing.Statistic.correlation(mat, revr)
      val aver = postprocessing.Statistic.aver(mat)
      val disp = postprocessing.Statistic.disp(mat, aver)
      assertEquals(cor, disp, 1e-10)
    }
  }

  def DaubechiesFullImageTest {
    import transform.DTransform._
    import image.Input._
    val inpName = dir + forDWT
    val img: BI = uploadImage(inpName)
    val mat = getColorsComponents(img, 2).map { _.map { _.toDouble } }
    for (ord <- 1 to 4) {
      val resDB: M = daubechiesForwardAndReverse(mat, order = ord)
      val cor: T = postprocessing.Statistic.correlation(mat, resDB)
      val aver: T = postprocessing.Statistic.aver(mat)
      val disp: T = postprocessing.Statistic.disp(mat, aver)
      assertEquals(cor, disp, eps = 20)
    }
  }

  def DaubechiesForwardImageTest {
    val inpName = dir + forDWT
    val img = image.Input.uploadImage(inpName)
    val resImg: BI = DaubechiesForwardImage(img, order = 1, "mat")
    visible(resImg, "Daubechies Forward Image Test")
  }

  def DaubechiesForwardImageWithRotateTest {
    val inpName = dir + forDWT
    val img = image.Input.uploadImage(inpName)
    val (imgTr, imgTheta) =
      DaubechiesForwardImageWithRotate(img, ord = 1)
    saveImage(imgTr, s"${dir}tr_im001.jpg", "jpg")
    saveImage(imgTheta, s"${dir}theta_im001.jpg", "jpg")
  }
}
