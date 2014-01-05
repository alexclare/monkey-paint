#!/bin/bash
# I never run this directly, it's mostly just to help me remember what
# commands I use to make movies out of multiple-image output

# sort in numeric order and pad with zeros
n=0; for i in `ls -1 | sort -n`; do mv $i $(printf %04d.png $n); n=$((n+1)); done

# frames to movie!
ffmpeg -i %04d.png output.mp4
