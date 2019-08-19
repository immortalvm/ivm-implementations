#!/bin/bash

# Based on https://stackoverflow.com/a/699613
die () {
    echo >&2 "$@"
    exit 1
}
[ "$#" -eq 1 ] || die "Usage: $0 <output directory>"

cd $1

#  Compute frame rate based on duration of first .wav file.
SAMPLE_RATE=$(ffprobe 2>&1 00000000.wav | egrep -o "[0-9]+ Hz" | egrep -o "[0-9]+")
WAV_SIZE=$(wc -c < 00000000.wav)  # More portable than using 'stat'
FRAME_RATE=$(( SAMPLE_RATE / ((WAV_SIZE - 44) / 4) ))

# Use libvorbis for audio, since AAC introduces noise in our sawtooth example.
# Add -y to overwrite existing files without question.
ffmpeg -f concat -safe 0 -i <(printf "file '%s'\n" $PWD/*.wav | egrep "/[0-9]{8}\.wav'") -c copy all.wav
ffmpeg -r $FRAME_RATE -f image2  -i %08d.png -i all.wav -vcodec libx264 -acodec libvorbis -crf 15  -pix_fmt yuv420p all.mp4
