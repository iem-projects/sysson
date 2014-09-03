![logo](https://raw.githubusercontent.com/iem-projects/sysson/master/src/main/resources/at/iem/sysson/SysSon-Logo_web_noshadow.png)

# SysSon

Repository for the software developed during the [SysSon Project](http://sysson.kug.ac.at/) at IEM Graz &ndash; SysSon is a systematic procedure to develop sonifications. Published under the GNU General Public License v3+.

Please see the `licenses` folder for details. It contains the license headers for all dependencies and transitive dependencies. For the binary release of SysSon, source code is not included but available via the respective OSS project pages, as indicated in the license files, or&mdash;in compliance with GPL/LGPL&mdash;on request via E-Mail. All source code with group-ID `de.sciss` is available from [github.com/Sciss](https://github.com/Sciss).

## Installation

__Please see [the wiki](https://github.com/iem-projects/sysson/wiki) for the most up-to-date information!__

The project is written in [Scala](http://www.scala-lang.org/) 2.11, 2.10 and builds with the [Simple Build Bool](http://www.scala-sbt.org/) (sbt) 0.13. The included shell script `sbt` is sufficient to build the project as it will automatically download and locally install Scala and sbt.

Furthermore, the project requires Java and SuperCollider.

### Building

First, open a terminal (bash), and go (`cd`) into the main project directory.

All libraries are automatically installed by sbt. 

Then to build the project on __Windows and Linux__:

    $ ./sbt assembly

The resulting file is `SysSon.jar` which can either be launched via `java -Xmx1024m -jar SysSon.jar`.

Or to build the project on __Mac OS X__:

    $ /.sbt appbundle

The resulting file is `SysSon.app` which can either be launched through double-click or via `open SysSon.app`.

### Developing

The recommended environment to develop the source code is [IntelliJ IDEA](http://www.jetbrains.com/idea/download/) Community Edition 13. After installing IDEA for the first time, the Scala plug-in must be installed. In IDEA, open `Preferences` > `IDE Settings` > `Plugins` > `Browse repositories...`. Find `Scala` and select `Download and Install`. It is also recommended to install the `SBT` plugin in the same manner. After the plugin installation, IDEA must be restarted.

A fresh IDEA project is setup by running the following command in the terminal: `./sbt gen-idea`. When this command has completed, the project can be opened in IDEA by choosing `File` > `Open...` and selecting the main project folder.

## Getting Started

See the different files in the `doc` directory. The API documentation for just SysSon can be created by running `./sbt doc` in the main directory. A complete API documentation including the dependencies (ScalaOSC, ScalaCollider etc.) can be created by moving into the `site` directory (`cd site`) and running `../sbt unidoc`. The result will be found in `site/target/scala-2.10/unidoc/index.html`.

After first launching the SysSon application, it is recommended to open and verify the preferences.

__Please see [the wiki](https://github.com/iem-projects/sysson/wiki) for the most up-to-date information!__
