#lang racket

(require "../lib.rkt")

(define time-list (make-list 65 0))

(define (pic type ids msg)
  (define pic-num
    (let loop ((pic-num (+ 1 (random 64))))
      (cond [(< (- (current-seconds) (list-ref time-list pic-num)) 120) (loop (+ 1 (random 64)))]
            [else pic-num])))

  (set! time-list (list-set time-list pic-num (current-seconds)))
  (define pic-name (~r pic-num #:min-width 2 #:pad-string "0"))

  ;;(displayln pic-name)

  (send-msg type (car ids) (format "[CQ:image,file=file:///data/pic/~A.png]" pic-name))

  )

(provide pic)
