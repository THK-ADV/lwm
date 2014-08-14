import play.PlayImport._
import play.PlayImport.PlayKeys._

name := """lwm"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.2"

libraryDependencies ++= Seq(
  cache,
  ws
)

TwirlKeys.templateImports += "utils.semantic._"

TwirlKeys.templateImports += "org.joda.time.DateTime"

TwirlKeys.templateImports += "javax.mail.Message"

libraryDependencies += "com.unboundid" % "unboundid-ldapsdk" % "2.3.6" withSources() withJavadoc()

libraryDependencies += "org.apache.directory.studio" % "org.apache.commons.codec" % "1.8" withSources()

libraryDependencies += "org.apache.jena" % "jena-arq" % "2.11.2" withSources() withJavadoc()

libraryDependencies += "org.apache.jena" % "jena-core" % "2.11.2" withSources() withJavadoc()

libraryDependencies += "org.apache.jena" % "jena-text" % "1.0.2" withSources() withJavadoc()

libraryDependencies += "org.apache.jena" % "jena-tdb" % "1.0.2" withSources() withJavadoc()

libraryDependencies += "org.apache.jena" % "jena-fuseki" % "1.0.2" withSources() withJavadoc()

libraryDependencies += "org.scalaj" %% "scalaj-http" % "0.3.16" withSources() withJavadoc()

libraryDependencies += "javax.mail" % "mail" % "1.5.0-b01" withSources() withJavadoc()