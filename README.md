# SysSon

Repository for the software developed during the [SysSon Project](http://sysson.kug.ac.at/) at IEM Graz &ndash; SysSon is a systematic procedure to develop sonifications. Published under the GNU General Public License v2+.

## Installation

The project is written in [Scala](http://www.scala-lang.org/) 2.10 and builds with the [Simple Build Bool](http://www.scala-sbt.org/) (sbt) 0.12.2. The included shell script `sbt` is sufficient to build the project as it will automatically download and locally install Scala and sbt.

Furthermore, the project requires Java and SuperCollider.

### Java

An installation of Java SDK 1.6 or higher is required. On OS X, you will already have this installed, on Windows and Linux you will have to make sure that [OpenJDK 6](http://openjdk.java.net/install/index.html) or [Oracle Java 6](http://www.oracle.com/technetwork/java/javase/downloads/index.html) ist installed. It has been reported that there might be issues with Scala-Swing and Java 7, so we recommend Java 6 (aka 1.6) over Java 7 (aka 1.7) at the moment.

### SuperCollider

The recommended [SuperCollider](http://supercollider.sourceforge.net/) version is 3.6.2.

__TODO__: Setting up `SC_HOME`.

### Building

First, open a terminal (bash), and go (`cd`) into the main project directory.

All libraries are automatically installed by sbt, with the exception of `netcdf` which must be installed manually:

    $ ./install_netcdf

Then to build the project on __Windows and Linux__:

    $ ./sbt assembly

The resulting file is `SysSon.jar` which can either be launched through double-click or via `java -jar SysSon.jar`.

Or to build the project on __Mac OS X__:

    $ /.sbt appbundle

The resulting file is `SysSon.app` which can either be launched through double-click or via `open SysSon.app`.

### Developing

The recommended environment to develop the source code is [IntelliJ IDEA](http://www.jetbrains.com/idea/download/) Community Edition 12. After installing IDEA for the first time, the Scala plug-in must be installed. In IDEA, open `Preferences` -> `IDE Settings` -> `Plugins` -> `Browse repositories...`. Find `Scala` and select `Download and Install`. It is also recommended to install the `SBT` plugin in the same manner. After the plugin installation, IDEA must be restarted.

A fresh IDEA project is setup by running the following command in the terminal: `./sbt gen-idea`. When this command has completed, the project can be opened in IDEA by choosing `File` -> `Open...` and selecting the main project folder.

## Getting Started

__TODO__: ... ... XXX
