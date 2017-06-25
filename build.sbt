version := "1.0"
scalaVersion := "2.12.2"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.7"
libraryDependencies += "ch.qos.logback" % "logback-core" % "1.1.7"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

libraryDependencies += "com.sun.media" % "jai_codec" % "1.0" from "file://./lib/jai_codec.jar"
libraryDependencies += "com.sun.media" % "jai_core" % "1.0" from "file://./lib/jai_core.jar"
libraryDependencies += "com.jhlabs" % "filters" % "2.0.235-1"
libraryDependencies += "net.imagej" % "ij" % "1.49c"

