#!/bin/sh

#./halite -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/Sample.jar"
#./halite -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/Durbinator-v1.0.jar"
## Use this to test early defense
#./halite -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/V32.jar" "java -jar jars/V30.jar" "java -jar jars/V29.jar"
./halite -d "384 256" "java -jar target/MyBot.jar" "java -jar jars/V42.jar" "java -jar jars/V44.jar" "java -jar jars/V43.jar"
mv *.hlt hlts/
