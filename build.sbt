organization        in ThisBuild  := "com.github.gdefacci.di"
version             in ThisBuild  := "0.2.0-SNAPSHOT"
scalaVersion        in ThisBuild  := "2.11.8"
crossScalaVersions  in ThisBuild  := Seq("2.11.8", "2.12.1") 
scalacOptions       in ThisBuild  ++= Seq("-unchecked", "-deprecation", "-feature")

lazy val runtime = Project("runtime", file("runtime"))
  .settings(Defaults.coreDefaultSettings)

lazy val scalaReflect = Def.setting { "org.scala-lang" % "scala-reflect" % scalaVersion.value }
  
lazy val macros = Project("macros", file("macros"))
  .settings(Defaults.coreDefaultSettings ++ Seq(
    libraryDependencies += scalaReflect.value,
    libraryDependencies += "com.github.jsr-330" % "core" % "1.4.0",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" 
  )).dependsOn(runtime)

lazy val root = Project("di", file(".")).aggregate(runtime, macros).settings(Seq(
  publishArtifact := false
))