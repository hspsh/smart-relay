#!/bin/sh

raco pkg install mqtt-client
racket -e "(begin (require (file \"smart-relay.rkt\")) (main $@))"

