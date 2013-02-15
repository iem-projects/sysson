import AssemblyKeys._

name := "SysSon"

version := "0.0.1-SNAPSHOT"

organization := "at.iem.sysson"

description := "Sonfication Server of the IEM SysSon project"

homepage := Some(url("https://github.com/iem-projects/sysson"))

licenses := Seq("GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt"))

// ---- scala compiler settings and libraries ----

scalaVersion := "2.10.0"

libraryDependencies ++= Seq(
  "de.sciss" %% "scalacolliderswing" % "1.4.+",
  "de.sciss" %% "swingtree" % "1.2.+",
  "de.sciss" %  "prefuse-core" % "0.21",
  "de.sciss" %  "scalacolliderugens-spec" % "1.4.+"
//  "de.sciss" %% "scalacolliderugens-api" % "1.4.+"
)

retrieveManaged := true

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-language:implicitConversions")

// ---- runtime settings ----

initialCommands in console := """import at.iem.sysson._
import Implicits._
import de.sciss.synth._
import ugen._
import Ops._
import de.sciss.osc.Implicits._
"""

// ---- build info source generator ----

buildInfoSettings

sourceGenerators in Compile <+= buildInfo

buildInfoKeys := Seq(name, organization, version, scalaVersion, description,
  BuildInfoKey.map(homepage) { case (k, opt) => k -> opt.get },
  BuildInfoKey.map(licenses) { case (_, Seq((lic, _))) => "license" -> lic }
)

buildInfoPackage <<= organization

// ---- packaging (making standalones) ----

// windows/linux

seq(assemblySettings: _*)

test in assembly := {}

target in assembly <<= baseDirectory    // make .jar file in the main directory

// mac os x

seq(appbundle.settings: _*)

appbundle.icon <<= (resourceDirectory in Compile, organization) { case (par, org) =>
  val icn = org.split('.').foldLeft(par)(_ / _) / "icon512.png"
  Some(icn)
}

appbundle.target <<= baseDirectory      // make .app bundle in the main directory
