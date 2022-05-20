name := "noteweb-paas-dsl"
organization := "com.noteinweb"
version := "1.0-SNAPSHOT"

scalaVersion := "2.13.8"

libraryDependencies ++= Seq(
  "org.scala-lang.modules"  %% "scala-parser-combinators" % "1.1.2",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4",

  "org.scalatest"           %% "scalatest"                % "3.0.8" % "test",
  "org.scalatestplus" %% "mockito-4-5" % "3.2.12.0" % "test"
)