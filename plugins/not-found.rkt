#lang racket

(require "../lib.rkt")

(define (not-found type ids msg)
  (send-msg type (car ids) "无法理解您的意思实在是非常抱歉"))

(provide not-found)
