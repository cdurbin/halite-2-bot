#!/bin/sh

lein uberjar
rm *.log
#./halite -d "240 160" "java -jar target/MyBot.jar log-stuff" "java -jar jars/run-to-corner.jar" "java -jar jars/run-to-corner.jar" "java -jar jars/run-to-corner.jar"
./halite -s 2426016305 -d "240 160" "java -jar target/MyBot.jar log-stuff" "java -jar jars/run-to-corner.jar" "java -jar jars/run-to-corner.jar" "java -jar jars/run-to-corner.jar"
mv *.hlt hlts/
