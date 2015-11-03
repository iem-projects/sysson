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
    case _                        => Some((resourceDirectory in Compile).value / "at"/"iem"/"sysson"/"application512.png")
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
