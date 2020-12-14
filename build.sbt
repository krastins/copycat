scalaVersion := "2.13.4"

name := "copycat"
organization := "krastins"
version := "1.0"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"


libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.1.1"
libraryDependencies += "de.sciss" %% "scalamidi" % "0.3.0"
