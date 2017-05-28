package main

import math._
import Basic._
import main.Constants._

object MatrixUpdate {

  /** @return 255.0*(x-mn)/(mx-mn)) or bordered value */
  def contrast(mat: M, mn: T, mx: T): M = {
    val c = 255.0 / (mx - mn)
    mapTT(mat,
      (x: T) =>
        if (x < mn) 0.0
        else if (x > mx) 255.0
        else (x - mn) * c)
  }

  /** @return 255*(x-mn)/(mx-mn)) or bordered value */
  def contrast(mat: MInt, mn: Int, mx: Int): MInt =
    mapII(mat,
      (x: Int) =>
        if (x < mn) 0
        else if (x > mx) 255
        else 255 * (x - mn) / (mx - mn))
}