enablePlugins(DebianPlugin)

lazy val authorName  = "Hanns Holger Rutz"
lazy val authorEMail = "contact@sciss.de"

maintainer in Debian := s"$authorName <$authorEMail>"

debianPackageDependencies in Debian += "java7-runtime"

// include all files in src/debian in the installed base directory
linuxPackageMappings in Debian ++= {
  val n     = (name            in Debian).value.toLowerCase
  val dir   = (sourceDirectory in Debian).value / "debian"
  val files = (dir * "*").filter(_.isFile).get  // direct child files inside `debian` folder
  files.map { fIn =>
    packageMapping(fIn -> s"/usr/share/$n/${fIn.name}")
      .withUser ("root")
      .withGroup("root")
      .withPerms("0644")  // http://help.unc.edu/help/how-to-use-unix-and-linux-file-permissions/
  }
}