import com.typesafe.sbt.packager.linux.LinuxPackageMapping

lazy val baseName       = "SysSon"
lazy val baseNameL      = baseName.toLowerCase
lazy val projectVersion = "1.15.0-SNAPSHOT"
lazy val mimaVersion    = "1.15.0"

lazy val commonSettings = Seq(
  name          := baseName,
  version       := projectVersion,
  organization  := "at.iem",
  description   := "Sonification platform of the IEM SysSon project",
  homepage      := Some(url(s"https://github.com/iem-projects/$baseNameL")),
  licenses      := Seq("GPL v3+" -> url("http://www.gnu.org/licenses/gpl-3.0.txt")),
  // ---- scala compiler settings and libraries ----
  scalaVersion  := scalaMainVersion,
  crossScalaVersions := Seq(scalaMainVersion, "2.11.11" /* , "2.10.6" */),
  // maven repository for NetCDF library
  resolvers    += "Unidata Releases" at "https://artifacts.unidata.ucar.edu/content/repositories/unidata-releases",
  // maven repository for Oracle BDB JE
  resolvers    += "Oracle Repository" at "http://download.oracle.com/maven",
  // maven repository for Typesafe Play
  resolvers += "Typesafe Maven Repository" at "http://repo.typesafe.com/typesafe/maven-releases/", // https://stackoverflow.com/questions/23979577
  fork in run := true,
  scalacOptions ++= {
    val xs = Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture")
    if (scalaVersion.value.startsWith("2.10")) xs else xs :+ "-Xlint:-stars-align,_"
  }
)

lazy val scalaMainVersion           = "2.12.2"

// ---- library versions ----

lazy val melliteVersion             = "2.13.1"
lazy val soundProcessesVersion      = "3.12.2"
lazy val lucreMatrixVersion         = "1.4.0-SNAPSHOT"
lazy val lucreSwingVersion          = "1.5.1"
lazy val lucreVersion               = "3.4.0"
lazy val scalaColliderVersion       = "1.22.3"
lazy val scalaColliderSwingVersion  = "1.33.1"
lazy val ugensVersion               = "1.16.4"
lazy val fileCacheVersion           = "0.3.4"
lazy val swingTreeVersion           = "0.1.2"
lazy val kollFlitzVersion           = "0.2.1"
lazy val sheetVersion               = "0.1.2"
lazy val slfVersion                 = "1.7.25"
lazy val fscapeVersion              = "2.7.1"
lazy val orsonpdfVersion            = "1.7"
lazy val webLaFVersion              = "2.1.3"

// ---- test libraries ----

lazy val scalaTestVersion           = "3.0.3"

// ---- other global constants

lazy val authorName                 = "Hanns Holger Rutz"
lazy val authorEMail                = "contact@sciss.de"

lazy val mainClazz                  = "at.iem.sysson.Main"

// ---- publishing ----

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  publishTo := {
    Some(if (isSnapshot.value)
      "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
    else
      "Sonatype Releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
    )
  },
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
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
)

// ---- packaging (making standalones) ----

//////////////// standalone fat jar
lazy val assemblySettings = Seq(
  test            in assembly := (),
  target          in assembly := baseDirectory.value,   // make .jar file in the main directory
  assemblyJarName in assembly := s"${name.value}.jar",
  mainClass in Compile := Some(mainClazz),
  assemblyMergeStrategy in assembly := {
  //  case "META-INF/MANIFEST.MF" => MergeStrategy.last
    case PathList("javax", "xml"                 , _ @ _*) => MergeStrategy.first  // conflict xml-apis vs. stax-api
    case PathList("org"  , "xmlpull"             , _ @ _*) => MergeStrategy.first  // from xstream I think
    case PathList("org"  , "w3c", "dom", "events", _ @ _*) => MergeStrategy.first  // bloody Apache Batik
    case other =>
      val oldStrategy = (assemblyMergeStrategy in assembly).value
      oldStrategy(other)
  }
)

lazy val root = Project(id = baseNameL, base = file("."))
  .enablePlugins(BuildInfoPlugin)
  .enablePlugins(JavaAppPackaging, DebianPlugin)
  .settings(commonSettings)
  .settings(publishSettings)
  .settings(assemblySettings)
  .settings(useNativeZip) // cf. https://github.com/sbt/sbt-native-packager/issues/334
  .settings(pkgUniversalSettings, pkgDebianSettings)
  .settings(
    libraryDependencies ++= Seq(
      "de.sciss" %% "mellite"                     % melliteVersion,             // computer music environment
      "de.sciss" %% "soundprocesses-core"         % soundProcessesVersion,      // computer music environment
      "de.sciss" %% "scalacollider"               % scalaColliderVersion,       // sound synthesis
      "de.sciss" %% "scalacolliderswing-core"     % scalaColliderSwingVersion,
      "de.sciss" %% "scalacolliderswing-plotting" % scalaColliderSwingVersion,  // plotting goodies
      "de.sciss" %% "scalacolliderugens-plugins"  % ugensVersion,               // third-party ugens
      // "at.iem"   %% "scalacollider-voices"        % voicesVersion,              // polyphony management
      "at.iem"   %% "lucrematrix"                 % lucreMatrixVersion,         // reactive matrix component and view
      "de.sciss" %% "lucreswing"                  % lucreSwingVersion,          // reactive widgets
      "de.sciss" %% "lucre-core"                  % lucreVersion,               // object model
      "de.sciss" %% "filecache-txn"               % fileCacheVersion,           // caching statistics of data files
      "de.sciss" %% "scala-swing-tree"            % swingTreeVersion,           // tree component
      "de.sciss" %% "kollflitz"                   % kollFlitzVersion,           // collection extensions
      "de.sciss" %% "sheet"                       % sheetVersion,               // Excel support
      "de.sciss" %% "fscape"                      % fscapeVersion,              // Offline processing
      "de.sciss" %  "weblaf-core"                 % webLaFVersion,              // look-and-feel
      "com.orsonpdf" % "orsonpdf"                 % orsonpdfVersion,  // silly JFreeChart doesn't support iText PDF
      "org.slf4j" % "slf4j-simple"                % slfVersion                  // logging (used by netcdf)
    ),
    libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
    mimaPreviousArtifacts := Set("at.iem" %% baseNameL % mimaVersion),
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
      |""".stripMargin,
    // ---- build info source generator ----
    buildInfoKeys := Seq(name, organization, version, scalaVersion, description,
      BuildInfoKey.map(homepage) { case (k, opt) => k -> opt.get },
      BuildInfoKey.map(licenses) { case (_, Seq((lic, _))) => "license" -> lic }
    ),
    buildInfoPackage := "at.iem.sysson"
  )

//////////////// universal (directory) installer
lazy val pkgUniversalSettings: Seq[sbt.Def.Setting[_]] = Seq(
  // NOTE: doesn't work on Windows, where we have to
  // provide manual file `SYSSON_config.txt` instead!
  javaOptions in Universal ++= Seq(
    // -J params will be added as jvm parameters
    "-J-Xmx1024m",
    "-J-XX:MaxPermSize=128M"
    // others will be added as app parameters
    // "-Dproperty=true",
  ),
  // Since our class path is very very long,
  // we use instead the wild-card, supported
  // by Java 6+. In the packaged script this
  // results in something like `java -cp "../lib/*" ...`.
  // NOTE: `in Universal` does not work. It therefore
  // also affects debian package building :-/
  // We need this settings for Windows.
  scriptClasspath /* in Universal */ := Seq("*")
)

//////////////// debian installer
lazy val pkgDebianSettings: Seq[sbt.Def.Setting[_]] = Seq(
  maintainer in Debian := s"$authorName <$authorEMail>",
  debianPackageDependencies in Debian += "java7-runtime",
  packageSummary in Debian := description.value,
  packageDescription in Debian :=
    """SysSon is a platform for the development and application
      | of sonification. It aims to be an integrative system that
      | serves different types of users, from domain scientists to
      | sonification researchers to composers and sound artists.
      | It therefore has an open nature capable of addressing different
      | usage scenarios.
      |""".stripMargin,
  // include all files in src/debian in the installed base directory
  linuxPackageMappings in Debian ++= {
    val n     = (name            in Debian).value.toLowerCase
    val dir   = (sourceDirectory in Debian).value / "debian"
    val f1    = (dir * "*").filter(_.isFile).get  // direct child files inside `debian` folder
    val f2    = ((dir / "doc") * "*").get
    //
    def readOnly(in: LinuxPackageMapping) =
      in.withUser ("root")
        .withGroup("root")
        .withPerms("0644")  // http://help.unc.edu/help/how-to-use-unix-and-linux-file-permissions/
    //
    val aux   = f1.map { fIn => packageMapping(fIn -> s"/usr/share/$n/${fIn.name}") }
    val doc   = f2.map { fIn => packageMapping(fIn -> s"/usr/share/doc/$n/${fIn.name}") }
    (aux ++ doc).map(readOnly)
  }
)
