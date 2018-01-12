#!/bin/sh

rm *.log
lein uberjar
export JVM_OPTS="-Dcom.sun.management.jmxremote -Dcom.sun.management.jmxremote.ssl=false -Dcom.sun.management.jmxremote.authenticate=false -Dcom.sun.management.jmxremote.port=1298"
./halite -s 1620639206 -d "240 160" "java $JVM_OPTS -jar target/MyBot.jar" "java -jar jars/V101.jar"
mv *.hlt hlts/
