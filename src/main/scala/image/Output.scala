package image

//import java.awt._
//import java.io.File
//import javax.imageio.ImageIO
import javax.swing._
//import java.awt.image._
import other.Types._

object Output {
  val logger = com.typesafe.scalalogging.Logger(getClass)

  def saveImage(im: java.awt.image.BufferedImage, fileName: String, format: String): Unit = {
    logger.info(s"try to save image to file with name ${fileName}")
    javax.imageio.ImageIO.write(im, format, new java.io.File(fileName))
  }

  def saveImage(mat: MInt, fileName: String): Unit = {
    val outImg = Operation.createTiffImage(mat)
    saveImage(outImg, fileName, "tif")
  }

  def saveImage(mat: M, fileName: String): Unit =
    saveImage(Operation.toColorMInt(mat), fileName)

  /** visualization image through frame */
  def visible(img: java.awt.image.BufferedImage, title: String) {
    val frame = new JFrame
    val icon = new ImageIcon(image.Operation.scale(img, 800.0 / img.getHeight))
    val label = new JLabel(icon)
    frame.getContentPane().add(label, java.awt.BorderLayout.CENTER)
    frame.pack()
    frame.setName(title)
    frame.setTitle(title)
    frame.setVisible(true)
    frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
  }

  def visualisationAndSaveMat(mat: M, frameName: String, fileName: String) {
    val gRes = Operation.toColorMInt(mat)
    val grayImg = Operation.createTiffImage(gRes)
    Output.visible(grayImg, frameName)
    Output.saveImage(grayImg, fileName, Input.defaultFormat)
  }
}