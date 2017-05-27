package image

import java.io.File
import javax.imageio.ImageIO
import java.awt.image._

import basic.Basic._

object Input {
  val format = "jpg"
  val imgType = BufferedImage.TYPE_INT_RGB 
  val imgColor = java.awt.Color.BLACK
  
 /**
  * @param name  name of file
  * @return image from file with name @name
  **/
  
  def getImage(name: String): BI = {
    val end = name.substring(name.lastIndexOf("."))
    if (end.equals("tif")) getTifImage(name) else
    ImageIO.read(new File(name))
  }
  
  def getTifImage(name: String): BI = {
    import com.sun.media.jai.codec._
    import java.awt.image.renderable.ParameterBlock 
    import javax.media.jai._
    val stream = new FileSeekableStream(name)
    val decodeParam = new TIFFDecodeParam(){this.setDecodePaletteAsShorts(true)}
    val params = new ParameterBlock() {this.add(stream)}
    JAI.create("tiff", params).getAsBufferedImage
  }
  
 /**
  * @param img some image
  * @return all pixels as array from raster data
  */
  def getPixels(img: BufferedImage): Array[Byte] = 
    img.getRaster.getDataBuffer( ).asInstanceOf[DataBufferByte].getData()
 
  private def getIntPixels(img: BufferedImage): Array[Int] = 
    getPixels(img).map{_.toInt}
  private def getColorPixels(img: BufferedImage): Array[Int] =
    getIntPixels(img).map{x => if (x >= 0) x else 127-x}
  def getMatGrayImage(img: BI): MInt = {
    val ar = getColorPixels(img)
    val m = img.getHeight; val n = img.getWidth
    val mat = createMInt(m, n)
    for (i <- 0 until m) {
      val ind = i*n
      for (j <- 0 until n)
        mat(i)(j) = ar(ind+j)
    }
    mat
  }
  
  def getColorsComponents(img: BI, colorID: Int): MInt = {
    val n = img.getWidth; val m = img.getHeight
    val shift = (colorID-1)*8
    val res = createMInt(m, n)
    for (y <- 0 until m; x <- 0 until n)
      res(y)(x) = (img.getRGB(x, y) >> shift)&255 
    res
  }
  
  def getColorsComponents(img: BI, cb: T, cg: T, cr: T) = {
    val n = img.getWidth; val m = img.getHeight
    val res = createMInt(m, n)
    for (y <- 0 until m; x <- 0 until n) {
      val rgb = img.getRGB(x, y)
      val b = rgb & 255
      val g = (rgb >> 8) & 255
      val r = (rgb >> 16) & 255
      res(y)(x) = (b*cb+g*cg+r*cr).floor.toInt
    }
    res
  }
}