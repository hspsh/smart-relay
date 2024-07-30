#lang racket/base

(require racket/place)

(provide make-toggle-relay)
(provide make-toggle-blinker-relay)

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

(define (make-toggle-blinker-relay toggle-relay
                                   #:delay [delay 3])
  (let ((blinker-thread #f))
    (lambda ()
      (if (and (thread? blinker-thread)
               (not (thread-dead? blinker-thread)))
          (if (thread-running? blinker-thread)
              (thread-suspend blinker-thread)
              (thread-resume blinker-thread))
          (set! blinker-thread
                (thread
                 (lambda ()
                   (let loop ()
                     (toggle-relay)
                     (sleep delay)
                     (loop))))))
      blinker-thread)))
