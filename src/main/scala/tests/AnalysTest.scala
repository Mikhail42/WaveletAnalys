package tests

import math._ 

import Base._
import basic.Basic._
import image._

object AnalysTest {
  val time = System.nanoTime()
  def printlnTime(s: String) = {
    println(s + " " +  (System.nanoTime()-time)/1e6.toInt + " ms")
  }
  /** vessels accentuation test (0 until 180 by 10 degree) */
  def vesselSegmentTest {
    val fileName = dir+forVessel
    val img = image.Operation.scale(Input.getImage(fileName), 1) 
    printlnTime("scale succesful")
    val invImg = preprocessing.Filter.fullInverse(img)
    printlnTime("fullInverse succesful")
    val updImg = preprocessing.Filter.MSR(invImg, r = 10)
    //TODO: image.Output.visible(updImg, "inv MSR")
    printlnTime("MSR succesful")
    val morle = new wavelets.Morlet(1, 3, 8) // a = 1.5
    val asVes = new wavelets.AsVessel(4, 1) // a = 1
    val mhat = new wavelets.MHAT(4, 1.5) // a = 1.5
    val fhat = new wavelets.FHAT(4, 2) // a = 2
    val gab = new wavelets.Gabor // a = 1
    //(new wavelets.OldAsVessel).specTransform(updImg, 2.2) // a=2.2
    //accentuation.Vessel.accent(updImg, r, s, "MAX") // r from 4 to 6 is norm
    
    for (s <- 2 to 2) { //; r <- 2 to 16 by 4) {
      try {
       // val res = image.Transform.cwt(morle, a=1.5, 1, updImg)
        val res = accentuation.Vessel.accent(updImg, 5, s, "MAX")
        //TODO: Output.visible(res._1, "WT")
        printlnTime("WT succesful")
        val out = preprocessing.Filter.deleteError(res._1, 2)
        printlnTime("delete error succesful")
        //val outInv = preprocessing.Filtr.inverse(out)
        //val delEr = preprocessing.DilateErose.eroseDilete(outInv)
        //val finalRes = preprocessing.Filtr.inverse(delEr)
        //TODO: Output.visible(out, "out")
        val gis = image.Analys.histogram(out)
        printlnTime("histogram succesful")
        val sum = gis.sum - gis(255) - gis(0)
        var locSum = 0
        var i = 254
        while (i>0 && locSum < 0.3*sum){
          locSum += gis(i)
          i -= 1
        }
        val bin = Operation.toBinary(out, i)
        printlnTime("toBinary succesful")
        //TODO: Output.visible(bin, "outF")
      } catch {
        case e: Exception => {}
      }
    }    
  }
  
  /** disk accentuation test */
  def diskTest{
    val fileName = dir+forDisk
    val img = image.Input.getImage(fileName)
    val res = accentuation.Disk.accent(img, r = 8)
    image.Output.visible(res, "Disc Test")
  }
}