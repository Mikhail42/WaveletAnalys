package image

import java.awt._
import java.awt.image._
import java.awt.geom.AffineTransform

import math._
import basic.Basic._

object Operation {
  
  def toGray(img: BI): BI = {
    val grF = new com.jhlabs.image.GrayFilter
    val resImg = new BI(img.getWidth, img.getHeight, BufferedImage.TYPE_BYTE_GRAY)
    grF.filter(img, resImg)
    resImg
  }
  
  /** forall cell in mat: cell => cell.toInt.max(0).min(255) */
  def toColorMInt(mat: M): MInt = mapTI(mat, (x: T) => x.toInt.max(0).min(255))
  
  /** full image's copy */
  def deepCopy(bi: BI): BI = {
    val cm: ColorModel = bi.getColorModel()
    val isAlphaPremultiplied = cm.isAlphaPremultiplied()
    val raster: WritableRaster = bi.copyData(null)
    new BI(cm, raster, isAlphaPremultiplied, null)
  }
  
  /** converted image to binary (1 byte gray color) */
  def toBinary(img: BI, white: Int = 160): BI = {
    import java.awt.image.BufferedImage
    val mat = mapII(imgToMInt(img), (x: Int) => if (x<white) 0 else 255)
    Operation.toImage(mat, BufferedImage.TYPE_BYTE_GRAY)
  }
  
  /** simple rotate image on theta degree.
   *  To rotate use #rotate */
  private def simpleRotate(img: BI, theta: T): BI = {
    val w = img.getWidth; val h = img.getHeight
    val angle: T = theta*Pi/180
    val at = new AffineTransform()
    
    at.translate(w, h)
    at.rotate(angle)
    at.translate(-w/2, -h/2)
    
    affineTransform(img, at, w, h)
  }
  
  /** rotate image on theta degree (theta in (-180; 180])
   *  resImg bigger img: (w, h)==size(img) -> (R,R)==size(resImg)
   **/
  def rotate(img: BI, theta: T): BI = {
    val resImg = simpleRotate(img, theta)
    val curW = resImg.getWidth; val curH = resImg.getHeight;
    val R = sqrt(sqr(img.getWidth)+sqr(img.getHeight)).toInt;
    resImg.getSubimage((curW-R)/2, (curH-R)/2, R, R)
  }
  
  /** rotate image from theta degree (theta in (-180; 180])
   *  new image as init image: (oldW,oldH) -> (R,R)==size(img) -> (oldW,oldH)
   **/
  def inverseRotate(img: BI, theta: T, oldW: Int, oldH: Int): BI = {
    val resImg = simpleRotate(img, theta)
    val curW = resImg.getWidth; val curH = resImg.getHeight;
    resImg.getSubimage((curW-oldW)/2, (curH-oldH)/2, oldW, oldH)
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
  
  /**
   * @param trans some affine transform
   * @param img used to obtain information about the image
   * @return new image on white background before transform
   */
  def affineTransform(img: BI, trans: AffineTransform, w: Int, h: Int): BI = {
    val resImg = new BI(2*w, 2*h, img.getType)
    val riGraphic = resImg.createGraphics()
    riGraphic.setBackground(Input.imgColor)
    riGraphic.clearRect(0, 0, 2*w, 2*h)
    resImg.getGraphics.asInstanceOf[ Graphics2D ].drawImage(img, trans, null)
    resImg
  }
  
  /** Let supMat == (red, green, blue) - components
   *  @return red<<16 + green<<8 + blue 
   **/
  @Deprecated
  def createImage(supMat: (MInt, MInt, MInt), imgType: Int): BI = {
    val r = supMat._1; val g = supMat._2; val b = supMat._3
    val h = r.length; val w = r(0).length;  
    val img = new BI(w, h, imgType)
    for(x <- 0 until w; y <- 0 until h){
      val c =  b(y)(x)       +
              (g(y)(x) << 8)  +
              (r(y)(x) << 16)
      img.setRGB(x, y, c)
    }
    img
  }
  
  /** Let supMat == (red, green, blue) - components
   *  @return red<<16 + green<<8 + blue 
   **/
  def createImage(mat: MInt, imgType: Int): BI = {
    val h = mat.length; val w = mat(0).length;  
    val img = new BI(w, h, imgType)
    for(x <- 0 until w; y <- 0 until h){
      val v = mat(y)(x).max(0).min(255)
      val c =  v       +
              (v << 8)  +
              (v << 16)
      img.setRGB(x, y, c)
    }
    img
  }
  
  /** @return mat<<16 + mat<<8 + mat 
   **/
  def toImage(mat: MInt, imgType: Int = Input.defaultImgType): BI = 
    Operation.createImage(mat, imgType)
  
  /** @see this.toImage */
  def matrixToImage(mat: M, imgType: Int = Input.defaultImgType): BI = 
    toImage(Operation.toColorMInt(mat), imgType)
  
  /** @see #basic.MathToolKit.correlation */
  def correlation(img1: BI, img2: BI, colorID: Int): T = {
    val mat1 = mapIT(Input.getColorsComponents(img1, colorID), (x: Int) => x.toDouble) 
    val mat2 = mapIT(Input.getColorsComponents(img2, colorID), (x: Int) => x.toDouble)
    val cor = basic.Statistic.correlation(mat1, mat2)
    cor
  }
  
  /** @see #basic.MathToolKit.disp */
  def disp(img: BI, colorID: Int): T = {
    val mat = mapIT(Input.getColorsComponents(img, colorID), (x: Int) => x.toDouble)
    val aver = basic.Statistic.aver(mat)
    basic.Statistic.disp(mat, aver)
  }
}