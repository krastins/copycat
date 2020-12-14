scalaVersion := "2.13.4"

name := "copycat"
organization := "krastins"
version := "1.0"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
  "org.scalatest" %% "scalatest" % "3.2.2" % "test",
  "org.typelevel" %% "cats-core" % "2.1.1",
  "de.sciss" %% "scalamidi" % "0.3.0",
  "com.chuusai" %% "shapeless" % "2.3.3"
)