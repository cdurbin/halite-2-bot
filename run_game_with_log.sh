#!/bin/sh

rm *.log
./halite -d "240 160" "java -jar target/MyBot.jar log-stuff" "java -jar jars/V117.jar"
mv *.hlt hlts/
