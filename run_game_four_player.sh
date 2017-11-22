#!/bin/sh

#./halite -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/Sample.jar"
#./halite -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/Durbinator-v1.0.jar"
## Use this to test early defense
#./halite -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/V15.jar" "java -jar jars/V16.jar" "java -jar jars/V14.jar"
./halite -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/V28.jar" "java -jar jars/V26.jar" "java -jar jars/V27.jar"
mv *.hlt hlts/
