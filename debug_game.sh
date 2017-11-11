#!/bin/sh

lein uberjar
#./halite -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/Sample.jar"
#./halite -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/Durbinator-v1.0.jar"
#./halite -t -s 1532922776 -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/V13.jar"
#./halite -t -s 2219499211 -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/V13.jar"
#./halite -t -s 2906390360 -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/V14.jar"
#./halite -t -s 2744142981 -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/V15.jar"
./halite -t -s 3835859866 -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/V15.jar"
mv *.hlt hlts/
