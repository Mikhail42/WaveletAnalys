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

}