package image

import main.Basic._
import main.Constants._

import java.io.File
import javax.imageio.ImageIO
import java.awt.image._

object Input {
  val defaultFormat = "jpg"
  val defaultImgType = BufferedImage.TYPE_INT_RGB
  val defaultBackgroundColor = java.awt.Color.BLACK

  /** @param name  name of file
   *  @return image from file with name @name
   */
  def uploadImage(name: String): BI = {
    val format = name.substring(name.lastIndexOf("."))
    if (format == "tif" || format == "tiff") uploadTiffImage(name)
    else ImageIO.read(new File(name))
  }

  def uploadTiffImage(name: String): BI = {
    val stream = new com.sun.media.jai.codec.FileSeekableStream(name)
    val params = new java.awt.image.renderable.ParameterBlock() { this.add(stream) }
    javax.media.jai.JAI.create("tiff", params).getAsBufferedImage
  }

  /** @param img some image
   *  @return all pixels as array from raster data
   */
  def getPixels(img: BufferedImage): Array[Byte] =
    img.getRaster.getDataBuffer.asInstanceOf[DataBufferByte].getData

  def grayMatFromImage(img: BI): MInt = {
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
    val n = img.getWidth; val m = img.getHeight
    val shift = (colorID - 1) * 8
    val res = createMInt(m, n)
    for (y <- 0 until m; x <- 0 until n)
      res(y)(x) = (img.getRGB(x, y) >> shift) & 255
    res
  }
  def getColorsComponents(img: BI): (MInt, MInt, MInt) = {
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
}