package tests

object Base {
  val logger = com.typesafe.scalalogging.Logger(getClass)

  private val resourcesDirectory = new java.io.File("src/main/resources")
  val dir = resourcesDirectory.getAbsolutePath() + "/"
  logger.info("base directory is {}", dir)

  val forVessel = "01_dr.jpg" //2//"image_023.jpg" //"
  val forDisk = "1.jpg" //"image_023.jpg" //""
  val forTiff = "01_g.tif"
  val forComp1 = "02_dr_out.jpg"
  val forComp2 = "02_dr.tif"
  val forMask = "01_g_full.jpg"
  val forMaskOut = "01_g.jpg"
  val forFullMask = "01_g_mask_full.jpg"
  val forCWT = "01_dr.jpg"
  val forDWT = "1.jpg"
  val forDirection = "126.jpg"
}