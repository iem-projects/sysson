![logo](https://raw.githubusercontent.com/iem-projects/sysson/master/src/main/resources/at/iem/sysson/SysSon-Logo_web_noshadow.png)

# SysSon

Repository for the software developed during the [SysSon Project](http://sysson.kug.ac.at/) at IEM Graz &ndash; SysSon is a systematic procedure to develop sonifications. SysSon is (C)opyright 2013&ndash;2015 by the Institute of Electronic Music and Acoustics (IEM), Graz. Written by Hanns Holger Rutz. This software is published under the GNU General Public License v3+.

Please see the `licenses` folder for details. It contains the license headers for all dependencies and transitive dependencies. For the binary release of SysSon, source code is not included but available via the respective OSS project pages, as indicated in the license files, or&mdash;in compliance with GPL/LGPL&mdash;on request via E-Mail. All source code with group-ID `de.sciss` is available from [github.com/Sciss](https://github.com/Sciss).

## Installation

__Please see [the wiki](https://github.com/iem-projects/sysson/wiki) for the most up-to-date information!__

The project is written in [Scala](http://www.scala-lang.org/) 2.11, 2.10 and builds with the [Simple Build Bool](http://www.scala-sbt.org/) (sbt) 0.13. The included shell script `sbt` is sufficient to build the project as it will automatically download and locally install Scala and sbt.

Furthermore, the project requires Java and SuperCollider.

### Building

First, open a terminal (bash), and go (`cd`) into the main project directory.

All libraries are automatically installed by sbt. 

Then to build the project:

    $ ./sbt assembly

The resulting file is `SysSon.jar`. The launcher script `SysSon.command` should be used to start the application. On OS X, you can open it with the `Terminal.app`.

### Developing

The recommended environment to develop the source code is [IntelliJ IDEA](http://www.jetbrains.com/idea/download/) Community Edition 14. After installing IDEA for the first time, the Scala plug-in must be installed (TODO: is this still necessary, or is the plug-in bundled now?). In IDEA, open `Preferences` > `IDE Settings` > `Plugins` > `Browse repositories...`. Find `Scala` and select `Download and Install`. After the plugin installation, IDEA must be restarted.

The project can now be imported into IntelliJ IDEA. It should recognize and offer a project model based on sbt.

## Getting Started

See the different files in the `doc` directory. The API documentation for just SysSon can be created by running `./sbt doc` in the main directory. A complete API documentation including the dependencies (ScalaOSC, ScalaCollider etc.) can be created by moving into the `site` directory (`cd site`) and running `../sbt unidoc`. The result will be found in `site/target/scala-2.11/unidoc/index.html`.

After first launching the SysSon application, it is recommended to open and verify the preferences.

__Please see [the wiki](https://github.com/iem-projects/sysson/wiki) for the most up-to-date information!__
