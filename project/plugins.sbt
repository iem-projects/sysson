addSbtPlugin( "com.eed3si9n" % "sbt-buildinfo" % "0.2.0" )  // provides version information to copy into main class

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.8.5")     // builds standalone jar for Windows and Linux

addSbtPlugin("de.sciss" % "sbt-appbundle" % "1.0.0")        // builds standalone application for OS X
