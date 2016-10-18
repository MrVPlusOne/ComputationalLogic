name := "ComputationalLogic"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies ++=
  Seq(
    "org.scalatest" %% "scalatest" % "3.0.0" % "test",
    "com.lihaoyi" %% "fastparse" % "0.3.7",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
    "org.scalacheck" %% "scalacheck" % "1.13.2" % "test"
  )