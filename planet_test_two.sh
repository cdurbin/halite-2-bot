#!/bin/sh

lein uberjar
rm *.log
./halite -d "240 160" "java -jar target/MyBot.jar log-stuff" "java -jar jars/run-to-corner.jar"

mv *.hlt hlts/
