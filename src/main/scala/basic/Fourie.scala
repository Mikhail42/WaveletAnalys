package basic

import math._
import basic.Basic._

/** some math methods
 *  @author Ionkin Mikhail 
 *  
 *  first author of this class: Christian Scheiblich (cscheiblich@gmail.com)
 *  @see https://github.com/cscheiblich/JWave tools.MathToolKit
 *  @see tests.MathTest.mathToolKitTests
 */
object Fourie {
    /** Returns a sampled array of #fun waves for given number of oscillations.
   * 
   * @author Christian Scheiblich (cscheiblich@gmail.com)
   * update: Ionkin Mikhail       (ionkinmikhail@gmail.com)
   * 
   * @param samplingRate
   * 		should be great than 2 and likely to be of 2^p | p E N
   * @param noOfOscillations
   * 		should be of natural numbers except zero
   * @param fun 
   * 		is function, e.g. sin or cos
   * @return sampled array keeping a number of @fun waves
   */
  @Deprecated 
  def createOscillation(samplingRate: Int, noOfOscillations: Int, fun: T => T): A = {
    val sR       = max(samplingRate, 2)
    val noOfOsc  = max(noOfOscillations, 2)
    val argMulti = 2*Pi*noOfOsc/sR
    for (i <- Array.range(0, sR)) yield fun(argMulti*i)
  }
  /** @see a def #createOscillation with a #fun = sin **/
  @Deprecated 
  def createSinOscillation(samplingRate: Int, noOfOscillations: Int) =
    createOscillation(samplingRate, noOfOscillations, sin)
  /** @see a def #createOscillation with a #fun = cos */
  @Deprecated 
  def createCosOscillation(samplingRate: Int, noOfOscillations: Int) =
    createOscillation(samplingRate, noOfOscillations, cos)
}