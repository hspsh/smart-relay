#!/bin/sh

if [[ ! $1 ]]; then
    echo "Usage: "
    echo "  ./create-smart-relay-containter.sh [mqtt-password]"
    exit;
fi;

docker run \
       -d \
       --rm \
       --network host \
       -v /dev/ttyUSB0:/dev/ttyUSB0 \
       -v smart-relay-racket-files:/.local \
       --privileged \
       racket-smart-relay-bash-nss-certs \
       smart-relay "#:host \"mqtt.hack\" #:username \"iot_basic\" #:password \"$1\" #:topic-name \"hsp/bobma/smart-relay\""

