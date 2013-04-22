import AssemblyKeys._

name := "SysSon"

version := "0.1.0-SNAPSHOT"

organization := "at.iem.sysson"

description := "Sonification Server of the IEM SysSon project"

homepage := Some(url("https://github.com/iem-projects/sysson"))

licenses := Seq("GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt"))

// ---- scala compiler settings and libraries ----

scalaVersion := "2.10.1"

libraryDependencies ++= {
  Seq(
    "de.sciss" %% "scalacollider" % "1.7.+",                    // client for SuperCollider
    "de.sciss" %% "scalacolliderswing" % "1.7.+",               // some graphical features for ScalaCollider
    "de.sciss" %  "scalacolliderugens-spec" % "1.5.+",          // UGen specs used in the patcher class
    "de.sciss" %% "scalaosc" % "1.1.1+",                        // Open Sound Control
    "de.sciss" %% "desktop" % "0.3.1+",                         // application framework
    "de.sciss" %  "intensitypalette" % "1.0.0",                 // colour palette
    "de.sciss" %% "filecache" % "0.2.+",                        // caching statistics of data files
    "org.jfree" % "jfreechart" % "1.0.14",                      // plotting
    "com.github.benhutchison" % "scalaswingcontrib" % "1.5"     // some GUI widgets
  )
}

retrieveManaged := true

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-language:implicitConversions")

// ---- runtime settings ----

initialCommands in console := """import at.iem.sysson._
import Implicits._
import de.sciss.synth._
import ugen._
import Ops._
import de.sciss.osc.Implicits._
import concurrent.duration._
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

appbundle.mainClass := Some("at.iem.sysson.gui.SwingApplication")

appbundle.javaOptions += "-Xmx2048m"

appbundle.target <<= baseDirectory      // make .app bundle in the main directory
