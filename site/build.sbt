def SYSSON_VERSION = "0.8.0"

val commonSettings = Seq(
  organization := "de.sciss"  // ghpages push only works with this, not "at.iem.sysson"...
  version      := SYSSON_VERSION,
  scalaVersion := "2.11.1"
)

// retrieveManaged in ThisBuild := true

val scalaOSC           = RootProject(uri("git://github.com/Sciss/ScalaOSC.git#v1.1.3"))

val scalaAudioFile     = RootProject(uri("git://github.com/Sciss/ScalaAudioFile.git#v1.4.2"))

val scalaColliderUGens = RootProject(uri("git://github.com/Sciss/ScalaColliderUGens.git#v1.9.0"))

val scalaCollider      = RootProject(uri("git://github.com/Sciss/ScalaCollider.git#v1.12.0"))

val sysson             = RootProject(uri("git://github.com/iem-projects/sysson.git#v0.8.0"))

git.gitCurrentBranch in ThisBuild := "master"

val root = (project in file("."))
  .settings(commonSettings: _*)
  .settings(unidocSettings: _*)
  .settings(site.settings ++ ghpages.settings: _*)
  .settings(
    name := "SysSon",
    site.addMappingsToSiteDir(mappings in (ScalaUnidoc, packageDoc), "latest/api"),
    git.remoteRepo := "git@github.com:iem-projects/sysson.git",
    scalacOptions in (Compile, doc) ++= Seq(
      "-skip-packages", Seq(
        "de.sciss.osc.impl", 
        "at.iem.sysson.gui.impl", 
        "at.iem.sysson.impl", 
        "at.iem.sysson.legacy",
        "at.iem.sysson.sound.impl",
        "at.iem.sysson.util",
        "de.sciss.synth.impl"
      ).mkString(":"),
      "-doc-title", "SysSon " + SYSSON_VERSION + " API"
    )
  )
  .aggregate(scalaOSC, scalaAudioFile, scalaColliderUGens, scalaCollider, sysson)

