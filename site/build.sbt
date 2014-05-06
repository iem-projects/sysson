val commonSettings = Seq(
  organization := "at.iem.sysson",
  version      := "0.1.0-SNAPSHOT",
  scalaVersion := "2.11.0"
)

retrieveManaged in ThisBuild := true

val scalaOSC           = RootProject(uri("git://github.com/Sciss/ScalaOSC.git#v1.1.3"))

val scalaAudioFile     = RootProject(uri("git://github.com/Sciss/ScalaAudioFile.git#v1.4.2"))

val scalaColliderUGens = RootProject(uri("git://github.com/Sciss/ScalaColliderUGens.git#v1.9.0"))

val scalaCollider      = RootProject(uri("git://github.com/Sciss/ScalaCollider.git#v1.12.0"))

val sysson             = RootProject(uri("git://github.com/iem-projects/sysson.git#v0.7.0"))

val root = (project in file("."))
  .settings(commonSettings: _*)
  .settings(unidocSettings: _*)
  // .settings(site.settings ++ ghpages.settings: _*)
  .settings(
    name := "SysSon",
    // site.addMappingsToSiteDir(mappings in (ScalaUnidoc, packageDoc), "latest/api"),
    // git.remoteRepo := "git@github.com:iem-projects/sysson.git",
    scalacOptions in (Compile, doc) ++= Seq("-skip-packages", "de.sciss.osc.impl")
  )
  .aggregate(scalaOSC, scalaAudioFile, scalaColliderUGens, scalaCollider, sysson)

