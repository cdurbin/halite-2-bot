#!/bin/sh

./halite -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/no-retreat.jar"

#lein uberjar
#./halite -s 1066395130 -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/no-retreat.jar"
mv *.hlt hlts/
