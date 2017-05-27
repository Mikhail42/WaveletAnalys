package basic

import math._
import Basic._
import basic.Constants._

object MatrixRotate {
            
  /**
    * rotate matrix 
    * @param mat -- matrix to rotate
    * @param theta -- rotate angle, from -180 to 180.
    * @param m -- usually, mat.length
    * @param n -- usually, mat(0).length
    * @see math definition as code
     * {{{
     * input: (y,x)
     * [m00 m01 m02] [x] = [m00x + m01y + m02]
     * [m10 m11 m12] [y] = [m10y + m11h + m12]
     * [0   0   0  ] [1] = [0    + 0    + 1  ]
     * 
     * 1. (y,x) => (y+h, x+w)
     * 2. (y,x) => [(c,-s); (s, c)]*(y,x)
     * 3. (y,x) => (y-h/2, x-w/2)
     * 
     * [1 0 w] [x] = [x + 0 + w]
     * [0 1 h] [y] = [0 + y + h]
     * [0 0 0] [1] = [0 + 0 + 1]
     * 
     * [c -s 0]   [1 0 -w/2]   [c -s  -cw/2+sh/2]
     * [s  c 0] * [0 1 -h/2] = [s  c  -sw/2-ch/2]
     * [0  0 1]		[0 0   1 ]	 [0  0       1    ]
     * 
     * [1 0 w]   [c -s  -cw/2+sh/2]   [c -s  -cw/2+sh/2+w]
     * [0 1 h] * [s  c  -sw/2-ch/2] = [s  c  -sw/2-ch/2+h]
     * [0 0 1] 	 [0  0       1    ]   [0  0        1     ]
     * 
     * [c -s  -cw/2+sh/2+w] [x]   [cx - sy -cw/2+sh/2+w]
     * [s  c  -sw/2-ch/2+h] [y] = [sx + cy -sw/2-ch/2+h]
     * [0  0       1      ] [1]   [        1           ]
     * }}}
     */
  def rotate(mat: M, theta: T, m: Int, n: Int): M = {
    val angle: T = Pi*theta/180
    val c = cos(angle); val s = sin(angle)
    
    val hM: T = 0.5*m; val hN: T = 0.5*n
    val addX = -c*hN + s*hM + n
    val addY = -s*hN - c*hM + m
    
    val res: M = createM(2*m, 2*n)
    for (y <- 0 until m; x <- 0 until n){
      val X = (c*x - s*y + addX).round.toInt
      val Y = (s*x + c*y + addY).round.toInt
      res(Y)(X) = mat(y)(x)  
    }
    res
  }
  
  def invRotate(mat: M, theta: T, m: Int, n: Int): M = {
    val angle: T = Pi*theta/180+Pi
    val c = cos(angle); val s = sin(angle)
    
    val hM: T = 0.5*m; val hN: T = 0.5*n
    val addX = -c*hN + s*hM + n
    val addY = -s*hN - c*hM + m
    
    val res: M = createWhiteMat(m, n)
    for (y <- 0 until m; x <- 0 until n){
      val X = (c*x - s*y + addX).round.toInt
      val Y = (s*x + c*y + addY).round.toInt
      res(y)(x) = mat(Y)(X)  
    }
    res
  }
}