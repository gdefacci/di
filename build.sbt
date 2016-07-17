organization := "org.obl"
name := "macro-di"
version := "0.1"

javaOptions ++= Seq("-Xss2048K", "-Xmx2g")

resolvers += "sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
scalaVersion := "2.11.8"
libraryDependencies <+= (scalaVersion)(sv => "org.scala-lang" % "scala-compiler" % sv)
libraryDependencies += "com.github.jsr-330" % "core" % "1.4.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"

