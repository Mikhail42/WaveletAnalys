package image

import math._
import other.Types._
import other.Constants._

import java.io.File
import javax.imageio.ImageIO
import java.awt.image._

object Convert {
  val logger = com.typesafe.scalalogging.Logger(getClass)

  /** @param img some image
   *  @return all pixels as array from raster data
   */
  def getPixels(img: BufferedImage): Array[Byte] = {
    logger.trace("try to get pixels fromimage")
    img.getRaster.getDataBuffer.asInstanceOf[DataBufferByte].getData
  }

  def grayMatFromImage(img: BI): MInt = {
    logger.trace("try to convert gray image to matrix")

    val ar = getPixels(img)
    val m = img.getHeight; val n = img.getWidth
    val mat = createMInt(m, n)
    for (i <- 0 until m) {
      val str = mat(i)
      val ind = i * n
      for (j <- 0 until n)
        str(j) = ar(ind + j) & 0xFF /*if (x >= 0) x else 127 - x */
    }
    mat
  }

  def getColorsComponents(img: BI, colorID: Int): MInt = {
    logger.debug(s"get ${colorID}th color comonents from image")

    val n = img.getWidth; val m = img.getHeight
    val shift = (colorID - 1) * 8
    val res = createMInt(m, n)
    for (y <- 0 until m; x <- 0 until n)
      res(y)(x) = (img.getRGB(x, y) >> shift) & 255
    res
  }
  def getColorsComponents(img: BI): (MInt, MInt, MInt) = {
    logger.info(s"get all colors comonents from image")

    val n = img.getWidth; val m = img.getHeight
    val B = createMInt(m, n) // >> 0
    val G = createMInt(m, n) // >> 8
    val R = createMInt(m, n) // >> 16
    for (y <- 0 until m; x <- 0 until n) {
      val rgb = img.getRGB(x, y)
      B(y)(x) = rgb & 0xFF
      G(y)(x) = (rgb >> 8) & 0xFF
      R(y)(x) = (rgb >> 16) & 0xFF
    }
    (R, G, B)
  }

  def getColorsComponents(img: BI, cb: T, cg: T, cr: T): MInt = {
    logger.debug(s"get specific color comonents from image: (b,g,r)=${(cb, cg, cr)}")

    val n = img.getWidth; val m = img.getHeight
    val res = createMInt(m, n)
    for (y <- 0 until m; x <- 0 until n) {
      val rgb = img.getRGB(x, y)
      val b = rgb & 255
      val g = (rgb >> 8) & 255
      val r = (rgb >> 16) & 255
      res(y)(x) = (b * cb + g * cg + r * cr).floor.toInt
    }
    res
  }

  def toBinaryArray(mat: MInt): Array[Byte] = {
    val m = mat.length; val n = mat(0).length
    val res = Array.ofDim[Byte](m * n)
    for (i <- 0 until m) {
      val ind0 = i * n
      val ar = mat(i)
      for (j <- 0 until n) {
        res(ind0 + j) = ar(j).toByte
      }
    }
    res
  }

  /** forall cell in mat: cell => cell.toInt.max(0).min(255) */
  def toColorMInt(mat: M): MInt = {
    logger.trace("convert matrix to color matrix (all components from 0 to 0xFF)")
    mapTI(mat, (x: T) => x.toInt.max(0).min(255))
  }

  /** full image's copy */
  def deepCopy(bi: BI): BI = {
    logger.trace("deep copy image started")
    val cm: java.awt.image.ColorModel = bi.getColorModel()
    val isAlphaPremultiplied = cm.isAlphaPremultiplied()
    val raster: java.awt.image.WritableRaster = bi.copyData(null)
    new BI(cm, raster, isAlphaPremultiplied, null)
  }

  /** converted image to binary (1 byte gray color) */
  def toBinary(img: BI, white: Int = 160): BI = {
    logger.debug(s"convert image to binary with border=${white}")
    val mat = mapII(imgToMInt(img), (x: Int) => if (x < white) 0 else 255)
    Convert.createTiffImage(mat)
  }

  /** Let supMat == (red, green, blue) - components
   *  @return red<<16 + green<<8 + blue
   */
  @Deprecated
  def createImage(supMat: (MInt, MInt, MInt), imgType: Int): BI = {
    logger.debug(s"create image from 3 matrixs with imgType=${imgType}")

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
    logger.trace(s"create tiff image from int matrix")

    val m = mat.length; val n = mat(0).length
    val resImg = new BI(n, m, java.awt.image.BufferedImage.TYPE_BYTE_GRAY)
    val wr = resImg.getRaster()
    for (x <- 0 until n; y <- 0 until m)
      wr.setSample(x, y, 0, mat(y)(x))
    resImg
  }

  def createTiffImage(mat: M): BI = {
    logger.trace(s"create tiff image from double matrix")
    val mat2 = mapTI(mat, (x: T) => x.round.toInt)
    createTiffImage(mat2)
  }
}