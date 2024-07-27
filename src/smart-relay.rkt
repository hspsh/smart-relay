(module smart-relay racket/base
  (require mqtt-client)
  (provide main)

  (define (make-send-to-dev data)
    (lambda (dev)
      (with-output-to-file dev
        #:exists 'truncate
        #:mode 'binary
        (lambda ()
          (write-bytes data)))))

  (define send-message-on
    (make-send-to-dev (bytes  #xA0 #x01 #x01 #xA2)))
  (define send-message-off
    (make-send-to-dev (bytes  #xA0 #x01 #x00 #xA1)))

  (define (make-toggle-relay dev)
    (let ((turn-on? #t))
      (lambda ()
        (displayln (format "Relay on ~a toggled" dev))
        (if turn-on?
            (send-message-on dev)
            (send-message-off dev))
        (set! turn-on? (not turn-on?)))))

  (define (main #:host        [host        "localhost"]
                #:username    [username    #f]
                #:password    [password    #f]
                #:client-name [client-name "smart-relay"]
                #:topic-name  [topic-name  "hsp/bobma/smart-relay"]
                #:relay-dev   [relay-dev   "/dev/ttyUSB0"])
    (define toggle-relay (make-toggle-relay relay-dev))
    (mqtt/with-client (host client-name)
      (mqtt/with-connection (#:keep-alive-interval 20
                             #:clean-session       #t
                             #:username            username
                             #:password            password)

        (mqtt/subscribe topic-name)
        (let loop ()
          (mqtt/with-message-recv (topic payload)
            (displayln (format "Message \"~a\" recieved on topic \"~a\"" payload topic))
            (cond
              ((string=? (bytes->string/utf-8 payload)
                         "toggle")
               (toggle-relay))))
          (loop))))))
