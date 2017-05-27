package basic

import math._
import Basic._
import basic.Constants._

object MatrixUpdate {
    
  /** @return 255.0*(x-mn)/(mx-mn)) or bordered value */
  def contrast(mat: M, mn: T, mx: T): M = 
    mapTT( mat, 
        (x: T) => 
          if (x<mn) 0.0 
          else 
            if (x>mx) 255.0 
            else 255.0*(x-mn)/(mx-mn))           
}