#!/bin/sh

lein uberjar
rm *.log
#./halite -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/Sample.jar"
#./halite -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/Durbinator-v1.0.jar"
## Use this to test early defense
#./halite -d "240 160" "java -jar target/MyBot.jar" "java -jar jars/V32.jar" "java -jar jars/V30.jar" "java -jar jars/V29.jar"
#./halite -d "384 256" "java -jar target/MyBot.jar" "java -jar jars/V47.jar" "java -jar jars/V44.jar" "java -jar jars/V55.jar"
#./halite -s 2734831486 -d "384 256" "java -jar target/MyBot.jar" "java -jar jars/V47.jar" "java -jar jars/V44.jar" "java -jar jars/V55.jar"
#./halite -s 833711020 -d "384 256" "java -jar target/MyBot.jar" "java -jar jars/V47.jar" "java -jar jars/V44.jar" "java -jar jars/V55.jar"

#./halite -d "384 256" "java -jar target/MyBot.jar log-stuff" "java -jar jars/V47.jar" "java -jar jars/V44.jar" "java -jar jars/V131.jar"

## Two rushers with old bots
#./halite -d "288 192" "java -jar target/MyBot.jar log-stuff" "java -jar jars/V47.jar" "java -jar jars/V44.jar" "java -jar jars/V131.jar"

#./halite -d "288 192" "java -jar target/MyBot.jar log-stuff" "java -jar jars/V147.jar" "java -jar jars/V148.jar" "java -jar jars/V131.jar"

#./halite -s 596501298 -d "384 256" "java -jar target/MyBot.jar log-stuff" "java -jar jars/V47.jar" "java -jar jars/V44.jar" "java -jar jars/V131.jar"

./halite -s 4274457560 -d "288 192" "java -jar target/MyBot.jar log-stuff" "java -jar jars/V147.jar" "java -jar jars/V148.jar" "java -jar jars/V131.jar"
mv *.hlt hlts/
