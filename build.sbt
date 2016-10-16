organization := "com.github.gdefacci"
name := "macro-di"
version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.8"

libraryDependencies <+= (scalaVersion)(sv => "org.scala-lang" % "scala-compiler" % sv)
libraryDependencies += "com.github.jsr-330" % "core" % "1.4.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"
// libraryDependencies += "com.github.gdefacci" %% "sgrafamento" % "0.1.0-SNAPSHOT" % "test"

