package image

import other.Types._
import math._
import java.awt.geom.AffineTransform

object AffineTransform {
  val logger = com.typesafe.scalalogging.Logger(getClass)

  /** simple rotate image on theta degree.
   *  To rotate use #rotate
   */
  private def simpleRotate(img: BI, theta: T): BI = {
    logger.info(s"simple rotate of image with theta=${theta}")
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
    logger.info(s"rotate of image with theta=${theta}")
    val resImg = simpleRotate(img, theta)
    val curW = resImg.getWidth; val curH = resImg.getHeight;
    val R = sqrt(sqr(img.getWidth) + sqr(img.getHeight)).toInt;
    resImg.getSubimage((curW - R) / 2, (curH - R) / 2, R, R)
  }

  /** rotate image from theta degree (theta in (-180; 180])
   *  new image as init image: (oldW,oldH) -> (R,R)==size(img) -> (oldW,oldH)
   */
  def inverseRotate(img: BI, theta: T, oldW: Int, oldH: Int): BI = {
    logger.info(s"inverseRotate of image with theta=${theta}")
    val resImg = simpleRotate(img, theta)
    val curW = resImg.getWidth; val curH = resImg.getHeight;
    resImg.getSubimage((curW - oldW) / 2, (curH - oldH) / 2, oldW, oldH)
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
    logger.info(s"rotate matrix on ${theta} degree")
    val angle: T = Pi * theta / 180
    val c = cos(angle); val s = sin(angle)

    val z = math.max(m, n)
    val cs = (0 until z).map { _ * c }
    val ss = (0 until z).map { _ * s }

    val addX = 0.5 * (-c * n + s * m) + n
    val addY = 0.5 * (-s * n - c * m) + m

    val res: M = createM(2 * m, 2 * n)
    for (y <- 0 until m) {
      val addX2 = -ss(y) + addX
      val addY2 = +cs(y) + addY
      for (x <- 0 until n) {
        val X = (cs(x) + addX2).round.toInt
        val Y = (ss(x) + addY2).round.toInt
        res(Y)(X) = mat(y)(x)
      }
    }
    res
  }

  /** @see #rotate
   */
  def invRotate(mat: M, theta: T, m: Int, n: Int): M = {
    logger.info(s"inverse rotate matrix on ${theta} degree")

    val angle: T = Pi * theta / 180 + Pi
    val c = cos(angle); val s = sin(angle)

    val z = math.max(m, n)
    val cs = (0 until z).map { _ * c }
    val ss = (0 until z).map { _ * s }

    val addX = 0.5 * (-c * n + s * m) + n
    val addY = 0.5 * (-s * n - c * m) + m

    val res: M = Array.fill[T](m, n)(255)

    for (y <- 0 until m) {
      val addX2 = -ss(y) + addX
      val addY2 = +cs(y) + addY
      for (x <- 0 until n) {
        val X = (cs(x) + addX2).round.toInt
        val Y = (ss(x) + addY2).round.toInt
        res(y)(x) = mat(Y)(X)
      }
    }
    res
  }

  /** @return scaled image */
  def scale(img: BI, scale: T): BI = {
    logger.info(s"scale image in ${scale} times")

    val newW = (img.getWidth * scale).round.toInt
    val newH = (img.getHeight * scale).round.toInt
    val res: BI = new BI(newW, newH, img.getType)
    val graph = res.createGraphics
    val transf = java.awt.geom.AffineTransform.getScaleInstance(scale, scale)
    graph.drawRenderedImage(img, transf)
    res
  }

  /** @param trans some affine transform
   *  @param img used to obtain information about the image
   *  @return new image on white background before transform
   */
  def affineTransform(img: BI, trans: AffineTransform, w: Int, h: Int): BI = {
    logger.info(s"affineTransform of image")
    val resImg = new BI(2 * w, 2 * h, img.getType)
    val riGraphic = resImg.createGraphics()
    riGraphic.setBackground(Input.defaultBackgroundColor)
    riGraphic.clearRect(0, 0, 2 * w, 2 * h)
    resImg.getGraphics.asInstanceOf[java.awt.Graphics2D].drawImage(img, trans, null)
    resImg
  }
}