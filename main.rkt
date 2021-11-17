#lang racket

(require web-server/servlet)
(require web-server/servlet-env)
(require json)
(require "router.rkt")

(define config (string->jsexpr
                (port->string
                 (open-input-file "config.json"))))

(define r-string (hash-ref config 'reg))

(define (req->json req)
  (bytes->jsexpr (request-post-data/raw req)))



(define (main-server req)
  (define message-info (req->json req))
  ;;(displayln message-info)
  ;;(displayln (string? (hash-ref message-info 'message_type)))
  (define type (hash-ref message-info 'message_type))
  (define ids
    (cond [(string=? type "private")
           (list (hash-ref (hash-ref message-info 'sender) 'user_id))]
          [(string=? type "group")
           (list (hash-ref message-info 'group_id)
                 (hash-ref (hash-ref message-info 'sender) 'user_id))]))
  (define msg  (hash-ref message-info 'raw_message))

  (route type ids (regexp-replace r-string msg ""))
  
  (response/empty))

(serve/servlet main-server #:port (hash-ref config 'listen-port)
               #:listen-ip (hash-ref config 'listen-ip)
               #:command-line? #t
               #:servlet-path "/")
