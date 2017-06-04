package transform

import math._
import other.Types._
import scala.annotation.tailrec

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
  @tailrec 
  def decompose(number: Int, acc: List[Int] = List[Int]()): List[Int] = {
    if (number <= 0) acc.reverse
    else {
      val log = log2(number)
      val num = 1 << log
      decompose(number - num, num::acc)
    }
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
  @tailrec
  def decomposeWithMaxBlock(number: Int, maxBlockSize: Int, acc: List[Int] = List[Int]()): List[Int] = {
    if (number <= 0) acc.reverse
    else if (number >= maxBlockSize) decomposeWithMaxBlock(number - maxBlockSize, maxBlockSize, maxBlockSize::acc) 
    else decompose(number, acc)
  }
}