#!/bin/sh

lein uberjar
rm -rf *.log
./halite -s 3544984409 -d "384 256" "java -jar target/MyBot.jar log-stuff" "java -jar jars/rusher.jar" "java -jar jars/no-retreat.jar" "java -jar jars/rusher.jar"
#./halite -d "384 256" "java -jar target/MyBot.jar" "java -jar jars/rusher.jar" "java -jar jars/no-retreat.jar" "java -jar jars/rusher.jar"
mv *.hlt hlts/
