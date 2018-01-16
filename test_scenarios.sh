#!/bin/sh

lein uberjar
rm *.log
## Make sure undock works correctly
./halite -s 2449320967 -d "384 256" "java -jar target/MyBot.jar log-stuff" "java -jar jars/V47.jar" "java -jar jars/V44.jar" "java -jar jars/V55.jar"
mv *.hlt hlts/
