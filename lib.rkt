#lang racket

(require net/url)
(require json)

(define config (string->jsexpr
                (port->string
                 (open-input-file "config.json"))))

(define send-ip (hash-ref config 'send-ip))
(define send-port (hash-ref config 'send-port))

(define (send-msg type id msg)
  (define send-u
    (cond [(string=? type "private")
           (url "http"
                #f
                send-ip
                send-port
                #t
                (list (path/param "send_msg" '()))
                (list (cons 'user_id (number->string id))
                      (cons 'message msg))
                #f)]
          [(string=? type "group")
           (url "http"
                #f
                send-ip
                send-port
                #t
                (list (path/param "send_msg" '()))
                (list (cons 'group_id (number->string id))
                      (cons 'message msg))
                #f)]))

  ;;(displayln send-u)
  (http-sendrecv/url send-u)
  (displayln (format "向~A ~A传送了一条信息：~A" type id msg)))

(provide send-msg)
                
