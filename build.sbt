name := "biggest-island"

version := "0.1"

scalaVersion := "2.11.11"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "3.9.5" % "test"
)

scalacOptions in Test ++= Seq("-Yrangepos")
        