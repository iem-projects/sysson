name := "SysSon"

version := "0.0.1-SNAPSHOT"

organization := "at.iem.sysson"

description := "Sonfication Server of the IEM SysSon project"

homepage := Some(url("https://github.com/iem-projects/sysson"))

licenses := Seq("GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt"))

// ---- scala compiler settings and libraries ----

scalaVersion := "2.10.0"

libraryDependencies += "de.sciss" %% "scalacolliderswing" % "1.3.1+"

retrieveManaged := true

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-language:implicitConversions")

// ---- runtime settings ----

initialCommands in console := """import at.iem.sysson._
import Implicits._
import de.sciss.synth._
import ugen._
import Ops._
boot()"""

// ---- build info source generator ----

buildInfoSettings

sourceGenerators in Compile <+= buildInfo

buildInfoKeys := Seq(name, organization, version, scalaVersion, description,
  BuildInfoKey.map(homepage) { case (k, opt) => k -> opt.get },
  BuildInfoKey.map(licenses) { case (_, Seq((lic, _))) => "license" -> lic }
)

buildInfoPackage <<= organization
