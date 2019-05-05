<IMG ALT="logo" SRC="https://raw.githubusercontent.com/iem-projects/sysson/master/src/main/resources/at/iem/sysson/SysSon-Logo_noshadow_566px.png" WIDTH="283">

# SysSon

[![Build Status](https://travis-ci.org/iem-projects/sysson.svg?branch=master)](https://travis-ci.org/iem-projects/sysson)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/at.iem/sysson_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/at.iem/sysson_2.12)

SysSon is a sonification platform developed at the Institute of Electronic Music and Acoustics (IEM) Graz. SysSon 
stands for a systematic procedure to develop sonifications. SysSon was created with funding by the Austrian Science 
Fund FWF ([project SysSon, P 24159](http://sysson.kug.ac.at/)). Further development funded by the Austrian Knowledge 
Transfer Centre (WTZ Süd).

SysSon is (C)opyright 2013&ndash;2017 by the Institute of Electronic Music and Acoustics (IEM), Graz. Written by 
Hanns Holger Rutz. This software is published under the GNU General Public License v3+.

Please see the `licenses` folder for details. It contains the license headers for all dependencies and transitive 
dependencies. For the binary release of SysSon, source code is not included but available via the respective OSS 
project pages, as indicated in the license files, or&mdash;in compliance with GPL/LGPL&mdash;on request via E-Mail. 
All source code with group-ID `de.sciss` is available from [github.com/Sciss](https://github.com/Sciss).

## Installation and Documentation

[The wiki](https://github.com/iem-projects/sysson/wiki) contains all relevant information to get started with SysSon:

- [Software Installation](https://github.com/iem-projects/sysson/wiki/Installation)
- [User Manual](https://github.com/iem-projects/sysson/wiki/Table-of-Contents)


### Building from Source

The project is written in [Scala](http://www.scala-lang.org/) 2.12, 2.11 and builds with 
[sbt](http://www.scala-sbt.org/). The included shell script `sbt` is sufficient to build the project as it will 
automatically download and locally install Scala and sbt.
First, open a terminal (bash), and go (`cd`) into the main project directory.

Then to build the project:

    $ ./sbt assembly

The resulting file is `SysSon.jar`.

Building cross-platform installer package:

    $ ./sbt universal:packageBin

Building Debian installer package:

    $ ./sbt debian:packageBin

### API Docs

The latest API docs are available here: https://iem-projects.github.io/sysson/latest/api/

### Linking

We are now publishing artifacts to Maven Central:

    "at.iem" %% "sysson" % "1.17.0"

