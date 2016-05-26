#!/bin/sh
# avconv -i test_image_out/frame-%d.png -r 25 -c:v libx264 -crf 19 out.mp4
avconv -i /home/hhrutz/Documents/projects/Anemone/minuten/bleeding_rsmp/bleeding-rsmp-%d.png -r 25 -c:v libx264 -crf 22 /home/hhrutz/Documents/projects/Anemone/minuten/out3.mp4
