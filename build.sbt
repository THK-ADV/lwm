import com.typesafe.sbt.SbtScalariform
import scalariform.formatter.preferences._
import play.PlayImport._
import play.PlayImport.PlayKeys._
import play.twirl.sbt.Import._
import sbt._
import Process._
import Keys._
import play._

scalariformSettings

ScalariformKeys.preferences := ScalariformKeys.preferences.value
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(DoubleIndentClassDeclaration, true)
  .setPreference(PreserveDanglingCloseParenthesis, true)
  .setPreference(RewriteArrowSymbols, true)
  .setPreference(AlignParameters, true)
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(DoubleIndentClassDeclaration, true)
  .setPreference(PlaceScaladocAsterisksBeneathSecondAsterisk, true)

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

libraryDependencies += "org.apache.jena" % "jena-arq" % "2.12.0" withSources() withJavadoc()

libraryDependencies += "org.apache.jena" % "jena-core" % "2.12.0" withSources() withJavadoc()

libraryDependencies += "org.apache.jena" % "jena-text" % "1.1.0" withSources() withJavadoc()

libraryDependencies += "org.apache.jena" % "jena-tdb" % "1.1.0" withSources() withJavadoc()

libraryDependencies += "org.apache.jena" % "jena-fuseki" % "1.1.0" withSources() withJavadoc()

libraryDependencies += "org.scalaj" %% "scalaj-http" % "0.3.16" withSources() withJavadoc()

libraryDependencies += "javax.mail" % "mail" % "1.5.0-b01" withSources() withJavadoc()

libraryDependencies += "org.scalatestplus" %% "play" % "1.1.0" % "test" withSources() withJavadoc()

libraryDependencies += "org.mockito" % "mockito-all" % "1.9.5" withSources() withJavadoc()

libraryDependencies += "joda-time" % "joda-time" % "2.4" withSources() withJavadoc()

libraryDependencies += "de.jollyday" % "jollyday" % "0.4.7" withSources() withJavadoc()
