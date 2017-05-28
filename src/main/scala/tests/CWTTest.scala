package tests

import math._ 



import main.Basic._
import image._

import Base._
      

object CWTTest {
      
  def cwtTests {
    import wavelets._
    val filename = dir + forCWT
    val img = image.Input.getImage(filename)
    for (id <- 1 to 1; a <- 0.4 to 3 by 0.4) {
      def cwt(wave: ICWavelet) = 
        image.Transform.cwt(wave, a, id, img)
      cwt(new AsLongVessel(3, a))
      cwt(new Morlet(3, a))
      cwt(new Gauss)
      //cwt(new AsVessel(3, a))
      cwt(new MHAT(3, a))
      //cwt(new F800.0HAT(3, a))
      //cwt(new Gabor)
    }
  }
}