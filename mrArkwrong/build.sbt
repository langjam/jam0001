name := "Langjam001"

version := "0.1"

scalaVersion := "3.0.1"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio" % "1.0.10",
  "dev.zio" %% "zio-interop-cats" % "3.1.1.0",
  "org.typelevel" %% "cats-parse" % "0.3.4"
)
