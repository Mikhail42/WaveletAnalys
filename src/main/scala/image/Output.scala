package image

//import java.awt._
//import java.io.File
//import javax.imageio.ImageIO
import javax.swing._
//import java.awt.image._
import main.Basic._

object Output {

  def saveImage(im: java.awt.image.BufferedImage, fileName: String, format: String): Unit =
    javax.imageio.ImageIO.write(im, format, new java.io.File(fileName))

  def saveImage(mat: MInt, fileName: String, format: String, imgType: Int): Unit = {
    val outImg = Operation.toImage(mat, imgType)
    saveImage(outImg, fileName, format)
  }

  def saveImage(mat: M, fileName: String, format: String, imgType: Int): Unit =
    saveImage(Operation.toColorMInt(mat), fileName, format, imgType)

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
    val grayImg = Operation.createImage(gRes, Input.defaultImgType)
    Output.visible(grayImg, frameName)
    Output.saveImage(grayImg, fileName, Input.defaultFormat)
  }
}