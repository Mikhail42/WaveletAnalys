package image

import java.awt.geom.AffineTransform

import math._
import other.Types._
import other.Constants._

import java.io.File
import javax.imageio.ImageIO
import java.awt.image._

object Operation {

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

  private lazy val grayFilterJHLabs = new com.jhlabs.image.GrayFilter

  /** very long: 250 ms for disk image (1.jpg, 2048*1500) */
  def toGray(img: BI): BI = {
    val resImg = new BI(img.getWidth, img.getHeight, java.awt.image.BufferedImage.TYPE_BYTE_GRAY)
    grayFilterJHLabs.filter(img, resImg)
    resImg
  }

  /** forall cell in mat: cell => cell.toInt.max(0).min(255) */
  def toColorMInt(mat: M): MInt = mapTI(mat, (x: T) => x.toInt.max(0).min(255))

  /** full image's copy */
  def deepCopy(bi: BI): BI = {
    val cm: java.awt.image.ColorModel = bi.getColorModel()
    val isAlphaPremultiplied = cm.isAlphaPremultiplied()
    val raster: java.awt.image.WritableRaster = bi.copyData(null)
    new BI(cm, raster, isAlphaPremultiplied, null)
  }

  /** converted image to binary (1 byte gray color) */
  def toBinary(img: BI, white: Int = 160): BI = {
    val mat = mapII(imgToMInt(img), (x: Int) => if (x < white) 0 else 255)
    Operation.createTiffImage(mat)
  }

  /** simple rotate image on theta degree.
   *  To rotate use #rotate
   */
  private def simpleRotate(img: BI, theta: T): BI = {
    val w = img.getWidth; val h = img.getHeight
    val angle: T = theta * Pi / 180
    val at = new AffineTransform()

    at.translate(w, h)
    at.rotate(angle)
    at.translate(-w / 2, -h / 2)

    affineTransform(img, at, w, h)
  }

  /** rotate image on theta degree (theta in (-180; 180])
   *  resImg bigger img: (w, h)==size(img) -> (R,R)==size(resImg)
   */
  def rotate(img: BI, theta: T): BI = {
    val resImg = simpleRotate(img, theta)
    val curW = resImg.getWidth; val curH = resImg.getHeight;
    val R = sqrt(sqr(img.getWidth) + sqr(img.getHeight)).toInt;
    resImg.getSubimage((curW - R) / 2, (curH - R) / 2, R, R)
  }

  /** rotate image from theta degree (theta in (-180; 180])
   *  new image as init image: (oldW,oldH) -> (R,R)==size(img) -> (oldW,oldH)
   */
  def inverseRotate(img: BI, theta: T, oldW: Int, oldH: Int): BI = {
    val resImg = simpleRotate(img, theta)
    val curW = resImg.getWidth; val curH = resImg.getHeight;
    resImg.getSubimage((curW - oldW) / 2, (curH - oldH) / 2, oldW, oldH)
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

  /** @param trans some affine transform
   *  @param img used to obtain information about the image
   *  @return new image on white background before transform
   */
  def affineTransform(img: BI, trans: AffineTransform, w: Int, h: Int): BI = {
    val resImg = new BI(2 * w, 2 * h, img.getType)
    val riGraphic = resImg.createGraphics()
    riGraphic.setBackground(Input.defaultBackgroundColor)
    riGraphic.clearRect(0, 0, 2 * w, 2 * h)
    resImg.getGraphics.asInstanceOf[java.awt.Graphics2D].drawImage(img, trans, null)
    resImg
  }

  /** rotate matrix
   *  @param mat -- matrix to rotate
   *  @param theta -- rotate angle, from -180 to 180.
   *  @param m -- usually, mat.length
   *  @param n -- usually, mat(0).length
   *  @see math definition as code
   *  {{{
   *  input: (y,x)
   *  [m00 m01 m02] [x] = [m00x + m01y + m02]
   *  [m10 m11 m12] [y] = [m10y + m11h + m12]
   *  [0   0   0  ] [1] = [0    + 0    + 1  ]
   *
   *  1. (y,x) => (y+h, x+w)
   *  2. (y,x) => [(c,-s); (s, c)]*(y,x)
   *  3. (y,x) => (y-h/2, x-w/2)
   *
   *  [1 0 w] [x] = [x + 0 + w]
   *  [0 1 h] [y] = [0 + y + h]
   *  [0 0 0] [1] = [0 + 0 + 1]
   *
   *  [c -s 0]   [1 0 -w/2]   [c -s  -cw/2+sh/2]
   *  [s  c 0] * [0 1 -h/2] = [s  c  -sw/2-ch/2]
   *  [0  0 1]    [0 0   1 ]   [0  0       1    ]
   *
   *  [1 0 w]   [c -s  -cw/2+sh/2]   [c -s  -cw/2+sh/2+w]
   *  [0 1 h] * [s  c  -sw/2-ch/2] = [s  c  -sw/2-ch/2+h]
   *  [0 0 1]    [0  0       1    ]   [0  0        1     ]
   *
   *  [c -s  -cw/2+sh/2+w] [x]   [cx - sy -cw/2+sh/2+w]
   *  [s  c  -sw/2-ch/2+h] [y] = [sx + cy -sw/2-ch/2+h]
   *  [0  0       1      ] [1]   [        1           ]
   *  }}}
   */
  def rotate(mat: M, theta: T, m: Int, n: Int): M = {
    val angle: T = Pi * theta / 180
    val c = cos(angle); val s = sin(angle)

    val addX = 0.5 * (-c * n + s * m) + n
    val addY = 0.5 * (-s * n - c * m) + m

    val res: M = createM(2 * m, 2 * n)
    for (y <- 0 until m; x <- 0 until n) {
      val X = (c * x - s * y + addX).round.toInt
      val Y = (s * x + c * y + addY).round.toInt
      res(Y)(X) = mat(y)(x)
    }
    res
  }

  /** @see #rotate
   */
  def invRotate(mat: M, theta: T, m: Int, n: Int): M = {
    val angle: T = Pi * theta / 180 + Pi
    val c = cos(angle); val s = sin(angle)

    val addX = 0.5 * (-c * n + s * m) + n
    val addY = 0.5 * (-s * n - c * m) + m

    val res: M = createWhiteMat(m, n)
    for (y <- 0 until m; x <- 0 until n) {
      val X = (c * x - s * y + addX).round.toInt
      val Y = (s * x + c * y + addY).round.toInt
      res(y)(x) = mat(Y)(X)
    }
    res
  }

  /** Let supMat == (red, green, blue) - components
   *  @return red<<16 + green<<8 + blue
   */
  @Deprecated
  def createImage(supMat: (MInt, MInt, MInt), imgType: Int): BI = {
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
    val m = mat.length; val n = mat(0).length
    val resImg = new BI(n, m, java.awt.image.BufferedImage.TYPE_BYTE_GRAY)
    val wr = resImg.getRaster()
    for (x <- 0 until n; y <- 0 until m)
      wr.setSample(x, y, 0, mat(y)(x))
    resImg
  }
  def createTiffImage(mat: M): BI = {
    val mat2 = mapTI(mat, (x: T) => x.round.toInt)
    createTiffImage(mat2)
  }

}