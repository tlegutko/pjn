name := "pjn"

version := "1.0"

scalaVersion := "2.11.8"

scalacOptions ++= Seq("-feature")

libraryDependencies ++= Seq(
  "com.google.guava" % "guava" % "19.0",
  "net.java.dev.jna" % "jna" % "4.2.2",
  "org.apache.spark" %% "spark-core" % "1.6.1",
  "org.apache.spark" %% "spark-mllib" % "1.6.1"
)

