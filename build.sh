#!/bin/sh

cp $(guix time-machine -C channels.scm -- pack -f docker -m manifest.scm) docker-mqtt-smart-relay.tar.gz

