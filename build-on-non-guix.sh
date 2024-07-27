#!/bin/sh

docker run --privileged --name tmp --rm -it -v $(pwd):/smart-relay -w /smart-relay metacall/guix sh -c './build.sh'

