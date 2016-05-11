#!/bin/sh
sbt "run -s 0 -e 1499 -l 0.9 --offset-a 1 --offset-b 0 --filter filters/hp6.aif --kernel 32 --window 2 --rotate --shift-y 56 --gamma 0.5 /home/hhrutz/Documents/projects/Anemone/burned/video/frame-%d.png  ../Rays/image_out/frame-%d.png conv_image_out/frame-%d.png"
