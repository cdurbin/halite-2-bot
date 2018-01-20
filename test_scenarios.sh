#!/bin/sh

lein uberjar
rm *.log
## Make sure undock works correctly
#./halite -s 2449320967 -d "384 256" "java -jar target/MyBot.jar log-stuff" "java -jar jars/V47.jar" "java -jar jars/V44.jar" "java -jar jars/V55.jar"

## Lots of ships
#./halite -s 1446893507 -d "384 256" "java -jar target/MyBot.jar log-stuff" "java -jar jars/V47.jar" "java -jar jars/V44.jar" "java -jar jars/V55.jar"

## Defend testing
#./halite -s 1082503502 -d "384 256" "java -jar target/MyBot.jar" "java -jar jars/V47.jar" "java -jar jars/V44.jar" "java -jar jars/V55.jar"

## Good planet selection map
#./halite -s 1168232716 -d "240 160" "java -jar target/MyBot.jar log-stuff" "java -jar jars/V130.jar"
#./halite -s 1168232716 -d "240 160" "java -jar target/MyBot.jar log-stuff" "java -jar jars/V144.jar"
./halite -s 1168232716 -d "240 160" "java -jar target/MyBot.jar log-stuff" "java -jar jars/V162.jar"
mv *.hlt hlts/
