#!/bin/sh
avconv -i test_image_out/frame-%d.png -r 25 -c:v libx264 -crf 19 out.mp4
