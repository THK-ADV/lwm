name := """lwm"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  ws
)


libraryDependencies += "com.unboundid" % "unboundid-ldapsdk" % "2.3.6" withSources() withJavadoc()