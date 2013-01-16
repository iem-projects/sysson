name := "SysSonTest"

organization := "at.iem.sysson"

scalaVersion := "2.10.0"

libraryDependencies += "de.sciss" %% "scalacollider" % "1.3.1+"

retrieveManaged := true

scalacOptions ++= Seq( "-deprecation", "-unchecked" )

initialCommands in console := """import at.iem.sysson._
import Implicits._
import de.sciss.synth._
import ugen._
import Ops._
boot()"""
