#!/bin/sh
sbt "run resample -s 0 -e 1499 -n 0.04 -f 4 conv_image_out/frame-%d.png rsmp_image_out/frame-%d.png"
