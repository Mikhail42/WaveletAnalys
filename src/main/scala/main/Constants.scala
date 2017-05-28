package main

import math._

object Constants {
  val thetas = (0 until 360)
  val angles = thetas.map{_*Pi/180.0}
  val coss = angles.map{cos(_)}
  val sins = angles.map{sin(_)}
}