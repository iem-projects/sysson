import AssemblyKeys._

name          := "SysSon"

version       := "0.6.0-SNAPSHOT"

organization  := "at.iem.sysson"

description   := "Sonification Server of the IEM SysSon project"

homepage      := Some(url("https://github.com/iem-projects/sysson"))

licenses      := Seq("GPL v3+" -> url("http://www.gnu.org/licenses/gpl-3.0.txt"))

// ---- scala compiler settings and libraries ----

scalaVersion  := "2.10.4"

// maven repository for NetCDF library
resolvers    += "Unidata Releases" at "https://artifacts.unidata.ucar.edu/content/repositories/unidata-releases"

libraryDependencies ++= Seq(
  "de.sciss" %% "soundprocesses"          % "2.2.+",          // Computer music framework
  "de.sciss" %% "lucrematrix"             % "0.1.+",          // Reactive matrix component and view
  "de.sciss" %% "scalacolliderswing-core" % "1.14.+",         // audio (server) widgets
  "de.sciss" %% "scalainterpreterpane"    % "1.6.+",          // scala interpreter
  "de.sciss" %% "desktop-mac"             % "0.4.2+",         // application framework
  "de.sciss" %  "intensitypalette"        % "1.0.0",          // colour palette
  "de.sciss" %% "filecache-txn"           % "0.3.+",          // caching statistics of data files
  "de.sciss" %% "treetable-scala"         % "1.3.4+",         // GUI component
  "de.sciss" %% "lucrestm-bdb"            % "2.0.1+",         // database used for library
  "org.jfree" % "jfreechart"              % "1.0.17",         // plotting
  "de.sciss" %% "raphael-icons"           % "1.0.+",          // tool icons
  "com.github.benhutchison" % "scalaswingcontrib" % "1.5",    // tree component (to be replaced eventually...)
  "de.sciss" %% "guiflitz"                % "0.3.+",
  "de.sciss" %  "weblaf"                  % "1.27",           // cross-platform look-and-feel
  "org.slf4j" % "slf4j-simple"            % "1.7.6"           // logging (used by netcdf)
)

retrieveManaged := true

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

// ---- runtime settings ----

initialCommands in console :=
  """import at.iem.sysson._
    |import Implicits._
    |import de.sciss.synth._
    |import ugen._
    |import Ops._
    |import de.sciss.osc.Implicits._
    |import concurrent.duration._
    |import ucar.{nc2, ma2}
    |""".stripMargin

// ---- build info source generator ----

buildInfoSettings

sourceGenerators in Compile <+= buildInfo

buildInfoKeys := Seq(name, organization, version, scalaVersion, description,
  BuildInfoKey.map(homepage) { case (k, opt) => k -> opt.get },
  BuildInfoKey.map(licenses) { case (_, Seq((lic, _))) => "license" -> lic }
)

buildInfoPackage := organization.value

// ---- publishing ----

publishMavenStyle := true

publishTo :=
  Some(if (version.value endsWith "-SNAPSHOT")
    "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
  else
    "Sonatype Releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
  )

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := { val n = name.value
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

test      in assembly := ()

target    in assembly := baseDirectory.value    // make .jar file in the main directory

jarName   in assembly := s"${name.value}.jar"

mainClass in assembly := Some("at.iem.sysson.gui.SwingApplication")

//mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
//  {
//    case "META-INF/MANIFEST.MF" => MergeStrategy.last
//    case x => old(x)
//  }
//}

// mac os x

seq(appbundle.settings: _*)

appbundle.icon := {
  // XXX TODO: DRY
  val base  = (resourceDirectory in Compile).value
  val sub   = organization.value.split('.')
  val icn   = (base /: sub)(_ / _) / "application512.png"
  Some(icn)
}

appbundle.mainClass   := Some("at.iem.sysson.gui.SwingApplication")

appbundle.javaOptions += "-Xmx2048m"

appbundle.target      := baseDirectory.value      // make .app bundle in the main directory

appbundle.documents   += {
  // XXX TODO: DRY
  val base  = (resourceDirectory in Compile).value
  val sub   = organization.value.split('.')
  appbundle.Document(
    name       = "SysSon Workspace Document",
    role       = appbundle.Document.Editor,
    icon       = Some((base /: sub)(_ / _) / "workspace256.png"),
    extensions = Seq("sysson"),
    isPackage  = true
  )
}
