import de.johoop.jacoco4sbt._
import JacocoPlugin._
 
name := "json-lib"

organization := "com.identityblitz"

version := "0.1.0"

licenses := Seq("MIT License" -> url("http://www.opensource.org/licenses/mit-license.php"))

homepage := Some(url("https://github.com/brainysmith/conf-lib"))

scalaVersion := "2.10.3"

crossPaths := false

publishMavenStyle := true

publishArtifact in Test := false

resolvers += "Local Maven Repository" at Path.userHome.asFile.toURI.toURL + "/.m2/repository"

libraryDependencies ++= Seq(
  "org.codehaus.jackson" % "jackson-core-asl" % "1.9.10",
  "org.codehaus.jackson" % "jackson-mapper-asl" % "1.9.10",
  "org.scalatest" % "scalatest_2.10" % "2.0.1-SNAP" % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.2" % "test"
)

scalacOptions ++= List("-feature","-deprecation", "-unchecked")

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-l", "org.scalatest.tags.Slow")

//Code Coverage section
jacoco.settings

//itJacoco.settings

//Style Check section 
org.scalastyle.sbt.ScalastylePlugin.Settings
 
org.scalastyle.sbt.PluginKeys.config <<= baseDirectory { _ / "src/main/config" / "scalastyle-config.xml" }
