# anemone_15b3c-video

(C)opyright 2015&ndash2016 Hanns Holger Rutz. All rights reserved. Published under
the GNU General Public License v2+.

<img src="screenshot.jpg" alt="screenshot" width="512" height="384"/>

## sbt problem

There was a dependency resolution problem for `processing` with sbt 0.13.9. You may have run sbt twice.
We now use sbt 0.13.11, problem is probably gone.

## processing/gstreamer

- Processing 2 relies on Gstreamer 0.10, which is not available in Debian testing (Stretch).
- Processing 3 has a new -video library which when building from source in theory has a Gstreamer 1.x
  branch. However that project has a horrible build file, and published Maven artifacts for
  Processing 3 seem to be broken
- The best solution seems to be to install Gstreamer 0.10 on Debian Stretch which according to
  Internet sources can live happily next to Gstreamer 1.0.
- The following .deb files were downloaded from https://pkgs.org , using Jessie versions:
   - gstreamer-0.10-gconf
   - gstreamer-0.10-plugins-base
   - libgstreamer0.10-0
   - libgstreamer-plugins-base0.10-0
- After installing these packages with `dpkg -i`, using `sbt "run --list"` works

## keyboard control

There are two modes, 'render' and 'adjust'. System starts in 'render'. Pressing <kbd>A</kbd> toggles
between these two modes.

### adjust

- <kbd>A</kbd> leave adjustment mode, prints current values
- <kbd>Space</kbd> cycle image corner to adjust (four corners)
- <kbd>Left</kbd>, <kbd>Right</kbd>, <kbd>Up</kbd>, <kbd>Down</kbd> adjust corner

### render

- <kbd>A</kbd> enter adjustment mode
- <kbd>Escape</kbd> reset state to black, no timer
- <kbd>Enter</kbd> start program / timer
- <kbd>Right</kbd> stop program, select next algorithm
- <kbd>Right</kbd> stop program, select previous algorithm
- <kbd>Up</kbd> increase noise
- <kbd>Down</kbd> decrease noise
- <kbd>1</kbd> to <kbd>9</kbd> select algorithm
- <kbd>0</kbd> select black algorithm 
- <kbd>Control</kbd>-<kbd>Shift</kbd>-<kbd>F</kbd> toggle full screen
- <kbd>Control</kbd>-<kbd>Q</kbd> quit
