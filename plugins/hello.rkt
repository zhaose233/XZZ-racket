#lang racket

(require "../lib.rkt")

(define (hello type ids msg)
  ;;(displayln msg)
  
  (cond [(member 2994133243 ids)
         (send-msg type (car ids) "主人您好！请问有什么需要的吗？")]
        [else
         (send-msg type (car ids) "这位客人您好，如果有什么困扰的地方不必顾虑")]))

(provide hello)
