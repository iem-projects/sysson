#!/bin/sh
cd "`dirname $0`"
java -Xms2048m -Xmx2048m -XX:PermSize=256m -XX:MaxPermSize=512m -Dturbulence-s4-thresh=0.8 -server -cp SysSon.jar at.iem.sysson.turbulence.Turbulence --in-memory "$@"
