package image

import java.awt.geom.AffineTransform

import math._
import main.Basic._

object Operation {
  private lazy val grayFilterJHLabs = new com.jhlabs.image.GrayFilter

  /** very long: 250 ms for disk image (1.jpg, 2048*1500) */
  def toGray(img: BI): BI = {
    val resImg = new BI(img.getWidth, img.getHeight, java.awt.image.BufferedImage.TYPE_BYTE_GRAY)
    grayFilterJHLabs.filter(img, resImg)
    resImg
  }

  /** forall cell in mat: cell => cell.toInt.max(0).min(255) */
  def toColorMInt(mat: M): MInt = mapTI(mat, (x: T) => x.toInt.max(0).min(255))

  /** full image's copy */
  def deepCopy(bi: BI): BI = {
    val cm: java.awt.image.ColorModel = bi.getColorModel()
    val isAlphaPremultiplied = cm.isAlphaPremultiplied()
    val raster: java.awt.image.WritableRaster = bi.copyData(null)
    new BI(cm, raster, isAlphaPremultiplied, null)
  }

  /** converted image to binary (1 byte gray color) */
  def toBinary(img: BI, white: Int = 160): BI = {
    val mat = mapII(imgToMInt(img), (x: Int) => if (x < white) 0 else 255)
    Operation.createTiffImage(mat)
  }

  /** simple rotate image on theta degree.
   *  To rotate use #rotate
   */
  private def simpleRotate(img: BI, theta: T): BI = {
    val w = img.getWidth; val h = img.getHeight
    val angle: T = theta * Pi / 180
    val at = new AffineTransform()

    at.translate(w, h)
    at.rotate(angle)
    at.translate(-w / 2, -h / 2)

    affineTransform(img, at, w, h)
  }

  /** rotate image on theta degree (theta in (-180; 180])
   *  resImg bigger img: (w, h)==size(img) -> (R,R)==size(resImg)
   */
  def rotate(img: BI, theta: T): BI = {
    val resImg = simpleRotate(img, theta)
    val curW = resImg.getWidth; val curH = resImg.getHeight;
    val R = sqrt(sqr(img.getWidth) + sqr(img.getHeight)).toInt;
    resImg.getSubimage((curW - R) / 2, (curH - R) / 2, R, R)
  }

  /** rotate image from theta degree (theta in (-180; 180])
   *  new image as init image: (oldW,oldH) -> (R,R)==size(img) -> (oldW,oldH)
   */
  def inverseRotate(img: BI, theta: T, oldW: Int, oldH: Int): BI = {
    val resImg = simpleRotate(img, theta)
    val curW = resImg.getWidth; val curH = resImg.getHeight;
    resImg.getSubimage((curW - oldW) / 2, (curH - oldH) / 2, oldW, oldH)
  }

  /** @return scaled image */
  def scale(img: BI, scale: T): BI = {
    val newW = (img.getWidth * scale).round.toInt
    val newH = (img.getHeight * scale).round.toInt
    val res: BI = new BI(newW, newH, img.getType)
    val graph = res.createGraphics
    val transf = AffineTransform.getScaleInstance(scale, scale)
    graph.drawRenderedImage(img, transf)
    res
  }

  /** @param trans some affine transform
   *  @param img used to obtain information about the image
   *  @return new image on white background before transform
   */
  def affineTransform(img: BI, trans: AffineTransform, w: Int, h: Int): BI = {
    val resImg = new BI(2 * w, 2 * h, img.getType)
    val riGraphic = resImg.createGraphics()
    riGraphic.setBackground(Input.defaultBackgroundColor)
    riGraphic.clearRect(0, 0, 2 * w, 2 * h)
    resImg.getGraphics.asInstanceOf[java.awt.Graphics2D].drawImage(img, trans, null)
    resImg
  }

  /** Let supMat == (red, green, blue) - components
   *  @return red<<16 + green<<8 + blue
   */
  @Deprecated
  def createImage(supMat: (MInt, MInt, MInt), imgType: Int): BI = {
    val r = supMat._1; val g = supMat._2; val b = supMat._3
    val h = r.length; val w = r(0).length;
    val img = new BI(w, h, imgType)
    for (x <- 0 until w; y <- 0 until h) {
      val c = b(y)(x) +
        (g(y)(x) << 8) +
        (r(y)(x) << 16)
      img.setRGB(x, y, c)
    }
    img
  }

  def createTiffImage(mat: MInt): BI = {
    val m = mat.length; val n = mat(0).length
    val resImg = new BI(n, m, java.awt.image.BufferedImage.TYPE_BYTE_GRAY)
    val wr = resImg.getRaster()
    for (x <- 0 until n; y <- 0 until m)
      wr.setSample(x, y, 0, mat(y)(x))
    resImg
  }
  def createTiffImage(mat: M): BI = {
    val mat2 = mapTI(mat, (x: T) => x.round.toInt)
    createTiffImage(mat2)
  }

  /** @see #main.MathToolKit.correlation */
  def correlation(img1: BI, img2: BI, colorID: Int): T = {
    val mat1 = mapIT(Input.getColorsComponents(img1, colorID), (x: Int) => x.toDouble)
    val mat2 = mapIT(Input.getColorsComponents(img2, colorID), (x: Int) => x.toDouble)
    val cor = postprocessing.Statistic.correlation(mat1, mat2)
    cor
  }

  /** @see #main.MathToolKit.disp */
  def disp(img: BI, colorID: Int): T = {
    val mat = mapIT(Input.getColorsComponents(img, colorID), (x: Int) => x.toDouble)
    val aver = postprocessing.Statistic.aver(mat)
    postprocessing.Statistic.disp(mat, aver)
  }
}