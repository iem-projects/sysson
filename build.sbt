name          := "SysSon"

version       := "1.5.0"

organization  := "at.iem.sysson"

description   := "Sonification platform of the IEM SysSon project"

homepage      := Some(url("https://github.com/iem-projects/sysson"))

licenses      := Seq("GPL v3+" -> url("http://www.gnu.org/licenses/gpl-3.0.txt"))

// ---- scala compiler settings and libraries ----

scalaVersion  := "2.11.7"

crossScalaVersions := Seq("2.11.7", "2.10.5")

// maven repository for NetCDF library
resolvers    += "Unidata Releases" at "https://artifacts.unidata.ucar.edu/content/repositories/unidata-releases"

// maven repository for Oracle BDB JE
resolvers    += "Oracle Repository" at "http://download.oracle.com/maven"

fork in run := true

// ---- library versions ----

lazy val melliteVersion             = "1.7.0"
lazy val soundProcessesVersion      = "2.21.1"
lazy val lucreMatrixVersion         = "0.10.1"
lazy val lucreSwingVersion          = "0.9.1"
lazy val scalaColliderVersion       = "1.17.4"
lazy val scalaColliderSwingVersion  = "1.25.3"
lazy val ugensVersion               = "1.13.4"
lazy val fileCacheVersion           = "0.3.3"
lazy val swingTreeVersion           = "0.1.1"
lazy val kollFlitzVersion           = "0.2.0"
lazy val fscapeJobsVersion          = "1.5.0"
lazy val sheetVersion               = "0.1.0"
lazy val slfVersion                 = "1.7.12"

// ---- test libraries ----

lazy val scalaTestVersion           = "2.2.5"

libraryDependencies ++= Seq(
  "de.sciss" %% "mellite"                     % melliteVersion,             // computer music environment
  "de.sciss" %% "soundprocesses-core"         % soundProcessesVersion,      // computer music environment
  "de.sciss" %% "scalacollider"               % scalaColliderVersion,       // sound synthesis
  "de.sciss" %% "scalacolliderswing-core"     % scalaColliderSwingVersion,
  "de.sciss" %% "scalacolliderswing-plotting" % scalaColliderSwingVersion,  // plotting goodies
  "de.sciss" %% "scalacolliderugens-plugins"  % ugensVersion,               // third-party ugens
  "de.sciss" %% "lucrematrix"                 % lucreMatrixVersion,         // reactive matrix component and view
  "de.sciss" %% "lucreswing"                  % lucreSwingVersion,          // reactive widgets
  "de.sciss" %% "filecache-txn"               % fileCacheVersion,           // caching statistics of data files
  "de.sciss" %% "scala-swing-tree"            % swingTreeVersion,           // tree component
  "de.sciss" %% "kollflitz"                   % kollFlitzVersion,           // collection extensions
  "de.sciss" %% "fscapejobs"                  % fscapeJobsVersion,
  "de.sciss" %% "sheet"                       % sheetVersion,               // Excel support
  "org.slf4j" % "slf4j-simple"                % slfVersion                  // logging (used by netcdf)
)

libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test"

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

enablePlugins(BuildInfoPlugin)

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

lazy val authorName  = "Hanns Holger Rutz"
lazy val authorEMail = "contact@sciss.de"

pomExtra := { val n = name.value
<scm>
  <url>git@github.com:iem-projects/{n}.git</url>
  <connection>scm:git:git@github.com:iem-projects/{n}.git</connection>
</scm>
<developers>
  <developer>
    <id>sciss</id>
    <name>{authorName}</name>
    <url>http://www.sciss.de</url>
  </developer>
</developers>
}

// ---- packaging (making standalones) ----

lazy val mainClazz = Some("at.iem.sysson.Main")

//////////////// standalone fat jar

test            in assembly := ()
target          in assembly := baseDirectory.value    // make .jar file in the main directory
assemblyJarName in assembly := s"${name.value}.jar"

mainClass in Compile := mainClazz

assemblyMergeStrategy in assembly := {
//  case "META-INF/MANIFEST.MF" => MergeStrategy.last
  case PathList("javax", "xml", xs @ _*)   => MergeStrategy.first  // conflict xml-apis vs. stax-api
  case PathList("org", "xmlpull", xs @ _*) => MergeStrategy.first  // from xstream I think
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}

//////////////// universal (directory) installer

enablePlugins(JavaAppPackaging)

useNativeZip  // cf. https://github.com/sbt/sbt-native-packager/issues/334

//////////////// jdk-bundled installer - requires Oracle Java 8 to build

enablePlugins(JDKPackagerPlugin)

packageSummary      := s"${name.value} Application"
packageDescription  := description.value
maintainer          := authorName

// wixProductId              := "ce07be71-510d-414a-92d4-dff47631848a" // XXX TODO -- ???
// wixProductUpgradeId       := "4552fb0e-e257-4dbd-9ecb-dba9dbacf424" // XXX TODO -- ???

jdkAppIcon := {
  sys.props("os.name").toLowerCase match {
    case os if os.contains("mac") => None // "*.icns"
    case os if os.contains("win") => None // "*.ico"
    case _            /* linux */ => Some((resourceDirectory in Compile).value / "at"/"iem"/"sysson"/"application512.png")
  }
}

jdkPackagerType    := "all"

jdkPackagerJVMArgs := Seq("-Xmx1g")

jdkPackagerToolkit := SwingToolkit

jdkPackagerAssociations := Seq(FileAssociation(
  extension = "mllt", mimetype = "application/x-sysson", description = "SysSon Workspace",
  icon = Some((resourceDirectory in Compile).value / "at"/"iem"/"sysson"/"workspace256.png")))

// Yes, screw you, too.
(antPackagerTasks in JDKPackager) := (antPackagerTasks in JDKPackager).value orElse {
  for {
    f <- Some(file("/usr/lib/jvm/java-8-oracle/lib/ant-javafx.jar")) if f.exists()
  } yield f
}

writeAntBuild in JDKPackager := {
  val res  = (writeAntBuild in JDKPackager).value
  val main = (mainClass     in JDKPackager).value.getOrElse(sys.error("No main class specified"))
  val tgt  = (target        in JDKPackager).value
  val n    = (name          in JDKPackager).value
  val wm   = main.replace('.', '-')
  val desktop = 
    s"""[Desktop Entry]
       |Name=APPLICATION_NAME
       |Comment=APPLICATION_SUMMARY
       |Exec=/opt/APPLICATION_FS_NAME/APPLICATION_LAUNCHER_FILENAME
       |Icon=/opt/APPLICATION_FS_NAME/APPLICATION_LAUNCHER_FILENAME.png
       |Terminal=false
       |Type=Application
       |Categories=DEPLOY_BUNDLE_CATEGORY
       |DESKTOP_MIMES
       |StartupWMClass=$wm
       |""".stripMargin
  IO.write(tgt / "package" / "linux" / s"$n.desktop", desktop)
  res
}

//appbundle.icon := {
//  // XXX TODO: DRY
//  val base  = (resourceDirectory in Compile).value
//  val sub   = organization.value.split('.')
//  val icn   = (base /: sub)(_ / _) / "application512.png"
//  Some(icn)
//}
//
//appbundle.mainClass   := mainClazz
//
//// appbundle.javaOptions ++= Seq("-Xmx2048m", "-XX:MaxPermSize=512m")
//
//appbundle.javaOptions ++= Seq("-Xms2048m", "-Xmx2048m", "-XX:PermSize=256m", "-XX:MaxPermSize=512m", "-server")
//
//appbundle.target      := baseDirectory.value      // make .app bundle in the main directory
//
//appbundle.documents   += {
//  // XXX TODO: DRY
//  val base  = (resourceDirectory in Compile).value
//  val sub   = organization.value.split('.')
//  appbundle.Document(
//    name       = "SysSon Workspace Document",
//    role       = appbundle.Document.Editor,
//    icon       = Some((base /: sub)(_ / _) / "workspace256.png"),
//    extensions = Seq("sysson"),
//    isPackage  = true
//  )
//}
