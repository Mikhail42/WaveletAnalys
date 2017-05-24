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
object Decompose {
  
  /** The method converts a positive integer to the ancient Egyptian multipliers
   * which are actually the multipliers to display the number by a sum of the
   * largest possible powers of two. 
   * @see tests.MathTest#mathToolKitTests
   * e.g.:  
   * {{{ decompose(254).equals( List(128, 64, 32, 16, 8, 4, 2))} }}}
   */
  def decompose(number: Int): List[Int] = {
    if (number <= 0) return List[Int]()
    val log = log2(number)
    val num = 1 << log
    if (log > 0) num::decompose(number - num)
    else         List(num)
  }
  
  /**  splits the given length of the data array to a possible number of blocks in
   * block size and then handles the rest as the ancient egyptian decomposition:
   * e. g. 127 by block size 32 ends up as: 32 | 32 | 32 | 16 | 8 | 4 | 2 | 1.
	 * @param number
   *     the number that should be decompose; greater than block size
   * @param maxBlockSize
   *     the block size as a type of 2^p|p={1,2,4,..} that is first used
   *     blocks until a rest is left; smaller than parameter number.
   * @see tests.MathTest#mathToolKitTests
   */
  def decompose(number: Int, maxBlockSize: Int): List[Int] = {
    if (number <= 0) return List[Int]()
    val log = log2(number)
    val num = 1 << log       // usually block size. @see #decompose(#number)
    val noOfBlocks = if (num >= maxBlockSize) num / maxBlockSize else 1
    val elemToAdd = num / noOfBlocks 
    val curList = (for (i <- 0 until noOfBlocks) yield elemToAdd).toList
    if (log > 0) curList:::decompose(number - num, maxBlockSize)
    else         curList
  }
}