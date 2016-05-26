#!/bin/sh
sbt "run resample -s 1 -e 3378 -n 0.04 -f 2 ../../projects/Anemone/minuten/bleeding_out/bleeding-out-%d.png ../../projects/Anemone/minuten/bleeding_rsmp/bleeding-rsmp-%d.png"