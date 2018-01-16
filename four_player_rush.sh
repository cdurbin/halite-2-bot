#!/bin/sh

./halite -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/V33.jar" "java -jar jars/no-retreat.jar" "java -jar jars/V55.jar"

#lein uberjar
#./halite -s 1066395130 -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/no-retreat.jar"
mv *.hlt hlts/
