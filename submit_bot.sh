#!/bin/sh

(mkdir -p deploy && cp target/MyBot.jar deploy/)
(cd deploy && zip submission.zip ./*)
(cd ../hlt_client/hlt_client && ./client.py bot -b ../../ClojureBot/deploy/submission.zip)
