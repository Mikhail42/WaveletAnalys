package image

import javax.swing._
import other.Types._

object Output {
  val logger = com.typesafe.scalalogging.Logger(getClass)

  private val device = java.awt.GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice();
  private val screenWidth = device.getDisplayMode().getWidth();
  private val screenHeight = device.getDisplayMode().getHeight();

  def saveImage(im: java.awt.image.BufferedImage, fileName: String, format: String): Unit = {
    logger.debug(s"try to save image to file with name '${fileName}' and '${format}' format")
    javax.imageio.ImageIO.write(im, format, new java.io.File(fileName))
  }

  def saveImage(mat: MInt, fileName: String): Unit = {
    logger.debug(s"try to save int matrix as image to file with name '${fileName}'")
    val outImg = Convert.createTiffImage(mat)
    saveImage(outImg, fileName, "tif")
  }

  def saveImage(mat: M, fileName: String): Unit = {
    logger.debug(s"try to save double matrix as image to file with name '${fileName}'")
    saveImage(Convert.toColorMInt(mat), fileName)
  }

  /** visualization matrix through frame */
  def visible(mat: MInt, title: String) {
    logger.debug(s"visualization int matrix through frame with title='${title}'")
    visible(image.Convert.createTiffImage(mat), title)
  }

  /** visualization matrix through frame */
  def visible(mat: M, title: String) {
    logger.debug(s"visualization double matrix through frame with title='${title}'")
    visible(image.Convert.createTiffImage(mat), title)
  }

  /** visualization image through frame */
  def visible(img: java.awt.image.BufferedImage, title: String) {
    logger.debug(s"visualization image through frame with title='${title}'")
    val icon = new ImageIcon(img)
    val label = new JLabel(icon)
    val scroller = new JScrollPane(label)

    val frame = new JFrame
    frame.getContentPane().add(scroller, java.awt.BorderLayout.CENTER)
    frame.pack()
    frame.setName(title)
    frame.setTitle(title)
    frame.setVisible(true)
    frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
  }

  def visualisationAndSaveMat(mat: M, frameName: String, fileName: String) {
    logger.debug(s"try to visualisation with title='${frameName}' and save matrix to file with name '${fileName}'")
    val gRes = Convert.toColorMInt(mat)
    val grayImg = Convert.createTiffImage(gRes)
    Output.visible(grayImg, frameName)
    Output.saveImage(grayImg, fileName, Input.defaultFormat)
  }
}