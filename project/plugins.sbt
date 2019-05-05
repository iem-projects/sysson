addSbtPlugin("com.eed3si9n"     % "sbt-buildinfo"       % "0.9.0" )    // provides version information to copy into main class
addSbtPlugin("com.eed3si9n"     % "sbt-assembly"        % "0.14.9")    // builds standalone jar for Windows and Linux
addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.3.21")    // package binary artifacts
addSbtPlugin("com.typesafe"     % "sbt-mima-plugin"     % "0.3.0" )    // binary compatibility testing
