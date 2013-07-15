import AssemblyKeys._

name := "SysSon"

version := "0.3.0-SNAPSHOT"

organization := "at.iem.sysson" // "de.sciss"  // this should be "at.iem.sysson", but it would require setting up another account on Sonatype. so let's just use my org

description := "Sonification Server of the IEM SysSon project"

homepage := Some(url("https://github.com/iem-projects/sysson"))

licenses := Seq("GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt"))

// ---- scala compiler settings and libraries ----

scalaVersion := "2.10.2"

// maven repository for NetCDF library
resolvers += "Unidata Repository" at "https://artifacts.unidata.ucar.edu/content/repositories/unidata-releases"

libraryDependencies ++= {
  Seq(
    "edu.ucar" %  "netcdf"                  % "4.3.17",
    "de.sciss" %% "scalacollider"           % "1.8.+",          // client for SuperCollider
    "de.sciss" %% "scalacolliderswing"      % "1.8.+",          // some graphical features for ScalaCollider
    "de.sciss" %  "scalacolliderugens-spec" % "1.6.+",          // UGen specs used in the patcher class
    "de.sciss" %% "scalaosc"                % "1.1.1+",         // Open Sound Control
    "de.sciss" %% "desktop"                 % "0.3.2+",         // application framework
    "de.sciss" %  "intensitypalette"        % "1.0.0",          // colour palette
    "de.sciss" %% "filecache"               % "0.2.+",          // caching statistics of data files
    "de.sciss" %% "fileutil"                % "1.0.+",          // easy file name manipulation
    "de.sciss" %% "swingplus"               % "0.0.+",          // GUI helpers
    "org.jfree" % "jfreechart"              % "1.0.14",         // plotting
    "com.github.benhutchison" % "scalaswingcontrib" % "1.5",    // some GUI widgets
    "org.slf4j" % "slf4j-simple"            % "1.7.5"           // logging (used by netcdf)
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

// ---- publishing ----

publishMavenStyle := true

publishTo <<= version { v =>
  Some(if (v endsWith "-SNAPSHOT")
    "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
  else
    "Sonatype Releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
  )
}

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra <<= name { n =>
<scm>
  <url>git@github.com:iem-projects/{n}.git</url>
  <connection>scm:git:git@github.com:iem-projects/{n}.git</connection>
</scm>
<developers>
  <developer>
    <id>sciss</id>
    <name>Hanns Holger Rutz</name>
    <url>http://www.sciss.de</url>
  </developer>
</developers>
}

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
