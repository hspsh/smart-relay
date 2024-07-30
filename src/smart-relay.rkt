#lang racket/base

(require racket/place)
(require racket/match)
(require (file "utils.rkt"))

(provide main)

(define (main #:host        [host        "localhost"]
              #:username    [username    #f]
              #:password    [password    #f]
              #:client-name [client-name "smart-relay"]
              #:topic-name  [topic-name  "hsp/bobma/smart-relay"]
              #:relay-dev   [relay-dev   "/dev/ttyUSB0"])
  (define toggle-relay
    (make-toggle-relay relay-dev))

  (define toggle-blinker-relay
    (make-toggle-blinker-relay toggle-relay))

  (define (make-mqtt-worker)
    (let ((mqtt-worker (dynamic-place (string->path "mqtt-client.rkt") 'main)))
     (place-channel-put mqtt-worker (list host
                                          username
                                          password
                                          client-name
                                          topic-name))
     mqtt-worker))

  (define mqtt-worker (make-mqtt-worker))

  (let loop ()
    (match (place-channel-get mqtt-worker)
      ['toggle
       (toggle-relay)]
      ['toggle-blinker
       (toggle-blinker-relay)])
    (loop)))
