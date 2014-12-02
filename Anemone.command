#!/bin/sh
cd "`dirname $0`"
java -Xms2048m -Xmx2048m -XX:PermSize=256m -XX:MaxPermSize=512m -server -cp SysSon.jar at.iem.sysson.turbulence.Concert "$@"
