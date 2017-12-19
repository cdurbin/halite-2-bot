#!/bin/sh

./halite -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/rusher.jar"
mv *.hlt hlts/
