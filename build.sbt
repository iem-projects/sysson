import AssemblyKeys._

name          := "SysSon"

version       := "0.10.4"

organization  := "at.iem.sysson"

description   := "Sonification Server of the IEM SysSon project"

homepage      := Some(url("https://github.com/iem-projects/sysson"))

licenses      := Seq("GPL v3+" -> url("http://www.gnu.org/licenses/gpl-3.0.txt"))

// ---- scala compiler settings and libraries ----

scalaVersion  := "2.11.2"

crossScalaVersions := Seq("2.11.2", "2.10.4")

// maven repository for NetCDF library
resolvers    += "Unidata Releases" at "https://artifacts.unidata.ucar.edu/content/repositories/unidata-releases"

// maven repository for Oracle BDB JE
resolvers    += "Oracle Repository" at "http://download.oracle.com/maven"

fork in run := true

// ---- library versions ----

lazy val melliteVersion             = "0.13.1"

lazy val soundProcessesVersion      = "2.8.3"

lazy val lucreMatrixVersion         = "0.5.0"

// lazy val scalaColliderVersion       = "1.14.1"

lazy val scalaColliderSwingVersion  = "1.20.1"

lazy val ugensVersion               = "1.11.2"

lazy val fileCacheVersion           = "0.3.2"

lazy val swingTreeVersion           = "0.1.1"

//lazy val jfreechartVersion          = "1.0.19"

lazy val kollFlitzVersion           = "0.2.0"

lazy val fscapeJobsVersion          = "1.5.0"

lazy val slfVersion                 = "1.7.7"

lazy val scalaTestVersion           = "2.2.2"

// ---- Ivy probs ----

lazy val lucreSwingVersion          = "0.6.1"

libraryDependencies ++= Seq(
  "de.sciss" %% "lucreswing" % lucreSwingVersion,
  "de.sciss" %% "mellite"                     % melliteVersion,             // computer music environment
  "de.sciss" %% "soundprocesses"              % soundProcessesVersion,
//  "de.sciss" %% "scalacollider"               % scalaColliderVersion,
  "de.sciss" %% "scalacolliderswing-plotting" % scalaColliderSwingVersion,  // plotting goodies
  "de.sciss" %% "scalacolliderugens-plugins"  % ugensVersion,               // third-party ugens
  "de.sciss" %% "lucrematrix"                 % lucreMatrixVersion,         // reactive matrix component and view
  "de.sciss" %% "filecache-txn"               % fileCacheVersion,           // caching statistics of data files
  "de.sciss" %% "scala-swing-tree"            % swingTreeVersion,           // tree component
  "de.sciss" %% "kollflitz"                   % kollFlitzVersion,           // collection extensions
  "de.sciss" %% "fscapejobs"                  % fscapeJobsVersion,
//  "org.jfree" % "jfreechart"                % jfreechartVersion,          // plotting
  "org.slf4j" % "slf4j-simple"                % slfVersion                  // logging (used by netcdf)
)

libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test"

// retrieveManaged := true

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture")

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
  Some(if (isSnapshot.value)
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

lazy val mainClazz = Some("at.iem.sysson.Main")

// windows/linux

seq(assemblySettings: _*)

test      in assembly := ()

target    in assembly := baseDirectory.value    // make .jar file in the main directory

jarName   in assembly := s"${name.value}.jar"

mainClass in assembly := mainClazz

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

appbundle.mainClass   := mainClazz

// appbundle.javaOptions ++= Seq("-Xmx2048m", "-XX:MaxPermSize=512m")

appbundle.javaOptions ++= Seq("-Xms2048m", "-Xmx2048m", "-XX:PermSize=256m", "-XX:MaxPermSize=512m", "-server")

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
