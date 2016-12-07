organization := "com.github.gdefacci"
name := "macro-di"
version := "0.1.0-SNAPSHOT"

crossScalaVersions := Seq("2.11.8", "2.12.1") 

libraryDependencies <+= (scalaVersion)(sv => "org.scala-lang" % "scala-compiler" % sv)
libraryDependencies += "com.github.jsr-330" % "core" % "1.4.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
