#!/bin/sh

rm *.log
lein uberjar

./halite -s 2064523920 -d "240 160" "java -jar target/MyBot.jar log-stuff" "java -jar jars/rusher.jar"
mv *.hlt hlts/
