package image

import other.Types._
import other.Constants._

import java.io.File
import javax.imageio.ImageIO
import java.awt.image._

object Input {
  val logger = com.typesafe.scalalogging.Logger(getClass)

  val defaultFormat = "jpg"
  val defaultImgType = BufferedImage.TYPE_INT_RGB
  val defaultBackgroundColor = java.awt.Color.BLACK

  /** Subimage(w / 2 - 512, h / 2, 512, 512)
   *  warn: may be exception
   *  warn: visualization of the subimage
   */
  def uploadSubimage(name: String, size: Int = 512): BI = {
    logger.debug(s"try to upload subimage with name ${name}")

    val img = uploadImage(name)
    val w = img.getWidth; val h = img.getHeight
    val subImage = img.getSubimage(w / 2 - size, h / 2, size, size)
    image.Output.visible(subImage, s"subimage of ${name}")
    subImage
  }

  /** @param name  name of file
   *  @return image from file with name @name
   */
  def uploadImage(name: String): BI = {
    logger.debug(s"try to upload image with name ${name}")

    val format = name.substring(name.lastIndexOf(".") + 1)
    if (format == "tif" || format == "tiff") uploadTiffImage(name)
    else ImageIO.read(new File(name))
  }

  def uploadTiffImage(name: String): BI = {
    logger.debug(s"try to upload tiff image with name ${name}")

    val stream = new com.sun.media.jai.codec.FileSeekableStream(name)
    val params = new java.awt.image.renderable.ParameterBlock() { this.add(stream) }
    javax.media.jai.JAI.create("tiff", params).getAsBufferedImage
  }
}