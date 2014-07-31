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

libraryDependencies += "org.apache.directory.studio" % "org.apache.commons.codec" % "1.8" withSources()

libraryDependencies += "org.apache.jena" % "jena-arq" % "2.11.2" withSources() withJavadoc()

libraryDependencies += "org.apache.jena" % "jena-core" % "2.11.2" withSources() withJavadoc()

libraryDependencies += "org.apache.jena" % "jena-text" % "1.0.2" withSources() withJavadoc()

libraryDependencies += "org.apache.jena" % "jena-tdb" % "1.0.2" withSources() withJavadoc()

libraryDependencies += "org.apache.jena" % "jena-fuseki" % "1.0.2" withSources() withJavadoc()

libraryDependencies += "org.scalaj" %% "scalaj-http" % "0.3.16" withSources() withJavadoc()