#lang racket

(require "plugins/hello.rkt")
(require "plugins/not-found.rkt")
(require "plugins/help.rkt")
(require "plugins/echo.rkt")
(require "plugins/five.rkt")
(require "plugins/pic.rkt")

(define command-list
  (list
   (cons #rx"^hello"    hello)
   (cons #rx"^help"     help)
   (cons #rx"^echo( )*" echo)
   (cons #rx"^five( )*" five)
   (cons #rx"^pic"      pic)))

(define (route type ids msg)

  (let loop ((l command-list))
    (cond [(empty? l) (not-found type ids msg)]
          [else
           (define command-cons (car l))
           ;(displayln (regexp-match (car command-cons) msg))
           (if (regexp-match (car command-cons) msg)
               ((cdr command-cons) type ids
                                   (regexp-replace (car command-cons) msg ""))
               (loop (cdr l)))])))

(provide route)

