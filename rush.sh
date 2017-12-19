#!/bin/sh

./halite -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/no-retreat.jar"
mv *.hlt hlts/
