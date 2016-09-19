import UnidocKeys._

lazy val syssonVersion         = "1.9.0"
lazy val PROJECT_VERSION       = syssonVersion
lazy val baseName              = "SysSon"

lazy val melliteVersion        = "2.5.0"
lazy val soundProcessesVersion = "3.7.0"
lazy val oscVersion            = "1.1.5"
lazy val audioFileVersion      = "1.4.5"
lazy val scalaColliderVersion  = "1.20.1"
lazy val ugensVersion          = "1.15.3"
lazy val fscapeVersion         = "2.1.0"
lazy val lucreVersion          = "3.3.1"
lazy val matrixVersion         = "0.11.2"

val commonSettings = Seq(
  organization := "de.sciss",  // ghpages push only works with this, not "at.iem.sysson"...
  version      := PROJECT_VERSION,
  scalaVersion := "2.11.8"
)

val scalaOSC           = RootProject(uri(s"git://github.com/Sciss/ScalaOSC.git#v${oscVersion}"))
val scalaAudioFile     = RootProject(uri(s"git://github.com/Sciss/ScalaAudioFile.git#v${audioFileVersion}"))
val scalaColliderUGens = RootProject(uri(s"git://github.com/Sciss/ScalaColliderUGens.git#v${ugensVersion}"))
val scalaCollider      = RootProject(uri(s"git://github.com/Sciss/ScalaCollider.git#v${scalaColliderVersion}"))
val fscape             = RootProject(uri(s"git://github.com/Sciss/FScape-next.git#v${fscapeVersion}"))
val soundProcesses     = RootProject(uri(s"git://github.com/Sciss/SoundProcesses.git#v${soundProcessesVersion}"))
val mellite            = RootProject(uri(s"git://github.com/Sciss/Mellite.git#v${melliteVersion}"))
val lucreMatrix        = RootProject(uri(s"git://github.com/iem-projects/LucreMatrix.git#v${matrixVersion}"))
val sysson             = RootProject(uri(s"git://github.com/iem-projects/sysson.git#v${syssonVersion}"))

val lucreURI           = uri(s"git://github.com/Sciss/Lucre.git#v${lucreVersion}")
val lucreCore          = ProjectRef(lucreURI, "lucre-core")
val lucreExpr          = ProjectRef(lucreURI, "lucre-expr")
val lucreBdb6          = ProjectRef(lucreURI, "lucre-bdb6")

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
        "akka.stream.sciss",
        "de.sciss.fscape.graph.impl",
        "de.sciss.fscape.lucre.impl",
        "de.sciss.fscape.lucre.stream",
        "de.sciss.fscape.stream",
        "de.sciss.lucre.artifact.impl",
        "de.sciss.lucre.bitemp.impl",
        "de.sciss.lucre.confluent.impl",
        "de.sciss.lucre.event.impl",
        "de.sciss.lucre.expr.impl",
        "de.sciss.lucre.stm.impl",
        "de.sciss.lucre.synth.expr.impl",
        "de.sciss.lucre.synth.impl",
        "de.sciss.mellite.gui.impl",
        "de.sciss.mellite.impl",
        "de.sciss.osc.impl", 
        "de.sciss.synth.impl",
        "de.sciss.synth.proc.graph.impl",
        "de.sciss.synth.proc.gui.impl",
        "de.sciss.synth.proc.impl",
        "de.sciss.synth.ugen.impl",
        "de.sciss.tallin",
        "at.iem.sysson.gui.impl", 
        "at.iem.sysson.impl", 
        "at.iem.sysson.legacy",
        "at.iem.sysson.sound.impl",
        "at.iem.sysson.util",
        "de.sciss.lucre.matrix.impl",
        "de.sciss.lucre.matrix.gui.impl"
      ).mkString(":"),
      "-doc-title", s"${baseName} ${PROJECT_VERSION} API"
    ),
    unidocProjectFilter in (ScalaUnidoc, unidoc) := inAnyProject -- inProjects(lucreBdb6)
  )
  .aggregate(scalaOSC, scalaAudioFile, scalaColliderUGens, scalaCollider, fscape, soundProcesses, lucreCore, lucreExpr, mellite,
    lucreMatrix, sysson)
