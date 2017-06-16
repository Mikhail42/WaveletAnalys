package image

import javax.swing._
import other.Types._

object Output {
  val logger = com.typesafe.scalalogging.Logger(getClass)

  def saveImage(im: java.awt.image.BufferedImage, fileName: String, format: String): Unit = {
    logger.info(s"try to save image to file with name ${fileName}")
    javax.imageio.ImageIO.write(im, format, new java.io.File(fileName))
  }

  def saveImage(mat: MInt, fileName: String): Unit = {
    logger.info(s"try to save int matrix as image to file with name ${fileName}")
    val outImg = Operation.createTiffImage(mat)
    saveImage(outImg, fileName, "tif")
  }

  def saveImage(mat: M, fileName: String): Unit = {
    logger.info(s"try to save double matrix as image to file with name ${fileName}")
    saveImage(Operation.toColorMInt(mat), fileName)
  }

  /** visualization matrix through frame */
  def visible(mat: MInt, title: String) {
    visible(image.Operation.createTiffImage(mat), title)
  }

  /** visualization matrix through frame */
  def visible(mat: M, title: String) {
    visible(image.Operation.createTiffImage(mat), title)
  }

  /** visualization image through frame */
  def visible(img: java.awt.image.BufferedImage, title: String) {
    val imgToVisible =
      if (img.getHeight > 800) image.AffineTransform.scale(img, 800.0 / img.getHeight)
      else img
    val icon = new ImageIcon(imgToVisible)
    val label = new JLabel(icon)

    val frame = new JFrame
    frame.getContentPane().add(label, java.awt.BorderLayout.CENTER)
    frame.pack()
    frame.setName(title)
    frame.setTitle(title)
    frame.setVisible(true)
    frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
  }

  def visualisationAndSaveMat(mat: M, frameName: String, fileName: String) {
    logger.info(s"try to visualisation and save matrix to file with name ${fileName}")
    val gRes = Operation.toColorMInt(mat)
    val grayImg = Operation.createTiffImage(gRes)
    Output.visible(grayImg, frameName)
    Output.saveImage(grayImg, fileName, Input.defaultFormat)
  }
}