package transform

import other.Types._
import image._

object Transform {
  val logger = com.typesafe.scalalogging.Logger(getClass)

  def DaubechiesForwardImage(img: BI, order: Int, trans: String): BI = {
    logger.trace(s"DaubechiesForwardImage of image")
    val mat: M =
      mapIT(Operation.getColorsComponents(img, 2), (x: Int) => x.toDouble)
    val g: MInt =
      mapTI(transform.DTransform.daubechies(mat, order, trans), (x: T) => x.toInt)
    val resImg: BI = Operation.createTiffImage(g)
    resImg
  }

  /** @param ord -- order of Daubechies
   */
  def DaubechiesForwardImageWithRotate(img: BI, ord: Int = 1): (BI, BI) = {
    logger.debug(s"DaubechiesForwardImage of image with rotate with order=${ord}")
    import transform.DTransform._
    import image.Operation._
    val m = img.getHeight; val n = img.getWidth
    val resTr = createMInt(m, n)
    val resTheta = createMInt(m, n)
    for (theta <- 0 until 180 by 10) {
      println(theta)
      val imgT: BI = image.GeomAffineTransform.rotate(img, theta)
      val matT = image.Operation.getColorsComponents(imgT, 2).
        map { _.map { 255 - _.toDouble } }
      val gT: MInt = daubechies(matT, order = 1, transformID = "str").
        map { _.map { _.toInt } }
      val resImgT: BI = Operation.createTiffImage(gT)
      val resImg: BI = image.GeomAffineTransform.inverseRotate(resImgT, -theta, img.getWidth, img.getHeight)
      val locMat = image.Operation.getColorsComponents(resImg, 2)
      for (i <- 0 until m; j <- 0 until n)
        if (locMat(i)(j) > resTr(i)(j)) {
          resTr(i)(j) = locMat(i)(j)
          resTheta(i)(j) = theta
        }
    }
    val imgTr = Operation.createTiffImage(resTr)
    val imgTheta = Operation.createTiffImage(resTheta)
    (imgTr, imgTheta)
  }

  /** @param a -- scala
   *  @param id -- wave rotate => 1
   *                image rotate => 2
   */
  def cwt(wave: wavelets.ICWavelet, a: T, id: Int = 1, img: BI): (BI, BI, BI) = {
    logger.info(s"CWT of image with a=${a} and id=${id}")
    val cwt = new transform.CTransform(wave)
    def trMor: M => M = cwt.transform(_, 0, a)
    val wavename = wave.wavename
    val matInt = imgToMInt(img)

    val mat = matInt.map { _.map { _.toDouble } }
    val m = img.getHeight; val n = img.getWidth;

    def locTr: Int => M = if (id == 1) locTr1 else locTr2
    def locTr1(theta: Int): M =
      cwt.transform(mat, theta, a)
    def locTr2(theta: Int): M = {
      import Operation._
      val imgR: BI = image.GeomAffineTransform.rotate(img, theta)
      val matR: M = imgToM(imgR)
      val matTrR: MInt = mapTI(trMor(matR), (x: T) => x.toInt)
      val imgTrR: BI = createTiffImage(matTrR)
      val imgTr: BI = image.GeomAffineTransform.inverseRotate(imgTrR, -theta, img.getWidth, img.getHeight)
      imgToM(imgTr)
    }

    val resTr = createM(m, n)
    //val resTheta = image.Analys.direction(matInt, 5, 5)
    val resTheta = createMInt(m, n)
    for (theta <- 0 until 180 by 10) {
      println(s"$theta degree succesful")
      val lTr = locTr(theta)
      for (i <- 0 until m; j <- 0 until n)
        if (lTr(i)(j) > resTr(i)(j)) {
          resTr(i)(j) = lTr(i)(j)
          resTheta(i)(j) = theta
        }
    }

    def mediate(directly: MInt, resMatImg: MInt) {
      logger.info(s"mediate started")
      val white = postprocessing.Mediate.whiteBorder
      val mediateMat = createMBool(m, n)
      for (y <- 0 until m; x <- 0 until n)
        if (resMatImg(y)(x) > white) {
          val (yMed, xMed): (Int, Int) =
            postprocessing.Mediate.getMediateLine(resMatImg, x, y, directly(y)(x))
          if (sqr(yMed - y) + sqr(xMed - x) <= 2)
            mediateMat(yMed)(xMed) = true
        }

      for (y <- 0 until m; x <- 0 until n)
        if (!mediateMat(y)(x))
          resMatImg(y)(x) = 0
    }

    val res = resTr.map { _.map { _.toInt } }
    val imgTr: BI = Operation.createTiffImage(res)
    val imgTheta: BI = Operation.createTiffImage(resTheta)
    mediate(resTheta, res)
    val imgTr2: BI = Operation.createTiffImage(res)
    (imgTr, imgTheta, imgTr2)
  }
}