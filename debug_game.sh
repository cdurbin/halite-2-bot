#!/bin/sh

lein uberjar
#./halite -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/Sample.jar"
#./halite -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/Durbinator-v1.0.jar"
#./halite -t -s 1532922776 -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/V13.jar"
#./halite -t -s 2219499211 -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/V13.jar"
#./halite -t -s 2906390360 -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/V14.jar"
#./halite -t -s 2744142981 -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/V15.jar"
#./halite -t -s 3835859866 -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/V15.jar"
#./halite -t -s 1613985642 -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/V18.jar"
## Ships crashing turn 170
#./halite -t -s 1686906791 -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/V19.jar"
## Ships crashing in turn 148 - angles make no sense to me, shouldn't be able to crash.
#./halite -t -s 1686906791 -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/V19.jar"
#./halite -t -s 1454044965 -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/V19.jar"
#./halite -t -s 1674064583 -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/V20.jar"
#./halite -t -s 2155479063 -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/V20.jar"
#./halite -t -s 1404160205 -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/V21.jar"
#./halite -t -s 3322122737 -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/V21.jar"
#./halite -t -s 875037268 -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/V22.jar"
./halite -t -s 3678893107 -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/V21.jar" "java -jar jars/V22.jar" "java -jar jars/V20.jar"

mv *.hlt hlts/
