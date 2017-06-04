version := "1.0"
scalaVersion := "2.12.2"

resolvers ++= Seq(
  "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/",
  "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/",
  "anormcypher" at "http://repo.anormcypher.org/",
  "Spray repository" at "http://repo.spray.io/"
)

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.7"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
//libraryDependencies += "com.jhlabs" % "filters" % "2.0.235-1"

libraryDependencies += "com.sun.media" % "jai_codec" % "1.0" from "file:///home/mio/Dropbox/workspace/Scala/Application/lib/jai_codec.jar"
libraryDependencies += "com.sun.media" % "jai_core" % "1.0" from "file:///home/mio/Dropbox/workspace/Scala/Application/lib/jai_core.jar"
libraryDependencies += "com.jhlabs" % "filters" % "1.0" from "file:///home/mio/Dropbox/workspace/Scala/Application/lib/filters.jar"

//libraryDependencies += "com.sun.media" %% "jai_codec" % "1.1.3"
//libraryDependencies += "com.sun.media" %% "jai_core" % "1.1.3"
//libraryDependencies += "junit" % "junit" % "4.12" % "test"
//libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
//libraryDependencies += "com.jhlabs" %% "filters" % "2.0.235-1"
//libraryDependencies += "com.jhlabs" %% "filters" % "2.0.235-1" % "test"

resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/releases"
libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.8.2"

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

parallelExecution in Test := true