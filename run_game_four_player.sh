#!/bin/sh

#./halite -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/Sample.jar"
#./halite -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/Durbinator-v1.0.jar"
./halite -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/V12.jar" "java -jar jars/V13.jar" "java -jar jars/V11.jar"
mv *.hlt hlts/
