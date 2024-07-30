#lang racket/base

(require racket/place)
(require racket/match)
(require mqtt-client)
(require (file "utils.rkt"))

(provide main)

(define (main c)
  (displayln "MQTT WORKER")
  (match (place-channel-get c)
    [(list host username password client-name topic-name)
     (mqtt/with-client (host client-name)
       (mqtt/with-connection (#:keep-alive-interval 20
                              #:clean-session       #t
                              #:username            username
                              #:password            password)


         (mqtt/with-qos ('qos-1)
           (mqtt/subscribe topic-name)
           (let loop ()
             (mqtt/with-message-recv (topic payload)
               (displayln (format "Message \"~a\" recieved on topic \"~a\"" payload topic))
               (let* ((payload (bytes->string/utf-8 payload))
                      (payload=? (lambda (what)
                                   (string=? payload
                                             what))))
                 (when (or (payload=? "toggle")
                           (payload=? "toggle-blinker"))
                   (place-channel-put c (string->symbol payload)))))
             (loop)))))]))

