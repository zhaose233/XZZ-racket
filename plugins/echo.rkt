#lang racket

(require "../lib.rkt")

(define (echo type ids msg)
  (send-msg type (car ids)
            (cond [(member 2994133243 ids)
                   (string-append "即然主人这么要求的话……\n"
                                  msg)]
                  [else
                   (string-append "请问您是说\n\""
                                  msg
                                  "\"\n吗？")])))

(provide echo)
