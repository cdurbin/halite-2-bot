#!/bin/sh

rm *.log
lein uberjar
./halite -s 3956038590 -d "240 160" "java -jar target/MyBot.jar log-stuff" "java -jar jars/no-retreat.jar"
mv *.hlt hlts/
