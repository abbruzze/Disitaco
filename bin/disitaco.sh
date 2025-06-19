#!/bin/bash

HOME=`dirname "$0"`
HOME=`cd "$HOME"; pwd -P`
LIB=$HOME/lib
CP=
OPT="-server -Xms64M -Xmx64M --sun-misc-unsafe-memory-access=allow --add-opens java.desktop/com.sun.media.sound=ALL-UNNAMED"
if [ ! -x $JAVA_HOME/bin/java ]; then
        JAVA=java
else
        JAVA=$JAVA_HOME/bin/java
fi
$JAVA $OPT -cp $CP -Ddisitaco.home=$HOME ucesoft.disitaco.ui.DisitacoUI "$@"
