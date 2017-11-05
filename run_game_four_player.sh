#!/bin/sh

#./halite -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/Sample.jar"
#./halite -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/Durbinator-v1.0.jar"
./halite -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/V9.jar" "java -jar jars/V8.jar" "java -jar jars/V7.jar"
mv *.hlt hlts/
