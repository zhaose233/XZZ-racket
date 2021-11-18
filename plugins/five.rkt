#lang racket

(require racket/draw)
(require "../lib.rkt")

(define board (make-list 225 0))

(define (set-board x y c)
  (define num (+ x (* (- y 1) 15) -1))
  (set! board
        (list-set board num c)))

(define (get-board x y)
  (define num (+ x (* (- y 1) 15) -1))
  (list-ref board num))

(define (win? x y c)

  (define ox-win? #f)
  (define len 0)
  (for ((i (range 1 16)))
    (if (= (get-board x i) c) (set! len (+ len 1))
        (set! len 0))
    (when (= len 5) (set! ox-win? #t)))

  (define oy-win? #f)
  (set! len 0)
  (for ((i (range 1 16)))
    (if (= (get-board i y) c) (set! len (+ len 1))
        (set! len 0))
    (when (= len 5) (set! oy-win? #t)))

  (define xy-win? #f)
  (set! len 0)
  (define x-tmp x)
  (define y-tmp y)
  (let loop ((x x-tmp) (y y-tmp))
    (cond [(or (= x 1) (= y 1)) (set! x-tmp x) (set! y-tmp y)]
          [else (loop (- x 1) (- y 1))]))
  (let loop ((x x-tmp) (y y-tmp))
    (cond [(or (= x 16) (= y 16)) #f]
          [else (if (= (get-board x y) c) (set! len (+ len 1))
                    (set! len 0))
                (when (= len 5) (set! xy-win? #t))
                (loop (+ x 1) (+ y 1))]))

  (define yx-win? #f)
  (set! len 0)
  (set! x-tmp x)
  (set! y-tmp y)
  (let loop ((x x-tmp) (y y-tmp))
    (cond [(or (= x 1) (= y 15)) (set! x-tmp x) (set! y-tmp y)]
          [else (loop (- x 1) (+ y 1))]))
  ;;(displayln x-tmp)
  ;;(displayln y-tmp)
  (let loop ((x x-tmp) (y y-tmp))
    (cond [(or (= x 16) (= y 0)) #f]
          [else (if (= (get-board x y) c) (set! len (+ len 1))
                    (set! len 0))
                ;;(displayln len)
                (when (= len 5) (set! yx-win? #t))
                (loop (+ x 1) (- y 1))]))

  (or ox-win? oy-win? xy-win? yx-win?))

(define (display-board)
  (let loop ((l board))
    (cond [(empty? l) (newline)]
          [else (display (cond [(= 1  (car l)) "O "]
                               [(= -1 (car l)) "X "]
                               [else           "- "]))
                (cond [(= (modulo (length (cdr l)) 15) 0) (newline)])

                (loop (cdr l))])))

(define pic-base (make-bitmap 565 565))
(define dc       (new bitmap-dc% (bitmap pic-base)))

(define (init-board)
  (define num-list '("1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15"))
  (send dc set-brush (make-color 255 178 102) 'solid)
  (send dc set-pen "black" 3 'solid)
  (send dc draw-rectangle 0 0 565 565)
  (for ((i (range 15)))
    (send dc draw-line 66 (+ 66 (* 31 i)) 500 (+ 66 (* 31 i)))
    (send dc draw-line (+ 66 (* 31 i)) 66 (+ 66 (* 31 i)) 500))
  (send dc set-font (make-font #:size 20 #:family 'roman
                               #:weight 'bold))
  (for ((i (range 15)) (num num-list))
    (send dc draw-text num 29 (+ 50 (* 31 i)))
    (send dc draw-text num 525 (+ 50 (* 31 i)))
    (send dc draw-text num (+ 52 (* 31 i)) 15)
    (send dc draw-text num (+ 52 (* 31 i)) 515)))

(define (draw-chess x y c)
  (cond [(= c 1) (send dc set-brush "white" 'solid)]
        [else    (send dc set-brush "black" 'solid)])
  (send dc draw-ellipse (+ 22 (* 31 x)) (+ 22 (* 31 y)) 27 27))

(define (draw-red x y)
  (send dc set-brush "red" 'solid)
  (send dc draw-ellipse (+ 30 (* 31 x)) (+ 30 (* 31 y)) 11 11))

(define (save-pic)
  (send pic-base save-file "/tmp/pic.png" 'png))

(define running? #f)
(define turn      1)
(define players '())
(define last-time (current-seconds))

(define (five type ids msg)
  ;;选取群消息
  (cond [(string=? type "group")
         ;;判断是否在进行中
         (cond [running?
                (define (turn-id) (cond [(= turn 1)  (list-ref players 0)]
                                        [(= turn -1) (list-ref players 1)]))
                ;;用最后一次落子的时间判断是否允许重开
                (cond [(regexp-match #rx"new" msg)
                       (cond [(> (- (current-seconds) last-time) 30)
                              (set! players (append '() (list (cadr ids))))
                              (set! board (make-list 225 0))
                              (set! turn 1)
                              (set! running? #f)
                              (send-msg type (car ids) (format "由于上次落子是半分种前，允许重开\n[CQ:at,qq=~A]想要开始一局新五子棋的对决\n有意加入的请发送 “maid five join” 加入" (cadr ids)))]
                             [else
                              (send-msg type (car ids) "有正在进行的对决，请稍等")])]
                      ;;是正要落子的人的消息
                      [(= (turn-id) (cadr ids))
                       (define x (string->number (car (regexp-match #rx"^[0-9]+" msg))))
                       (define msg-used (regexp-replace #rx"^[0-9]+( )*" msg ""))
                       (define y (string->number (car (regexp-match #rx"^[0-9]+" msg-used))))

                       ;;判断x与y是否合法
                       (cond [(and x y (= (get-board x y) 0) (>= x 1) (>= y 1) (<= x 15) (<= y 15))
                              (displayln x)
                              (displayln y)
                              (set-board x y turn)
                              (set! last-time (current-seconds))
                              (draw-chess x y turn)
                              (draw-red   x y)
                              (save-pic)
                              (draw-chess  x y turn)
                              (set! turn (* -1 turn))
                              ;;赢了
                              (cond [(win? x y (* turn -1))
                                     (send-msg type (car ids) (format "[CQ:image,file=file:///tmp/pic.png][CQ:at,qq=~A]，恭喜您胜利了" (cadr ids)))
                                     (set! board (make-list 225 0))
                                     (set! running? #f)
                                     (set! players '())
                                     (set! turn 1)]
                                    [else
                                     (display-board)
                                     (send-msg type (car ids) (format "[CQ:image,file=file:///tmp/pic.png]下面是[CQ:at,qq=~A]的回合" (turn-id)))])]
                             [else
                              (displayln x)
                              (displayln y)
                              (send-msg type (car ids) "你的输入有误！")])]
                      ;;不是正要落子的人的消息
                      [else
                       (send-msg type (car ids) "现在不是您的回合")])

                ]
               ;;不在运行时
               [else (cond [(= (length players) 0)
                            ;;收到新开
                            (cond [(regexp-match #rx"new" msg)
                                   (set! players (append '() (list (cadr ids))))
                                   (set! board (make-list 225 0))
                                   (set! turn 1)
                                   (set! running? #f)
                                   (send-msg type (car ids) (format "[CQ:at,qq=~A]想要开始一局新五子棋的对决\n有意加入的请发送 “maid five join” 加入" (cadr ids)))
                                   ]
                                  [else
                                   (send-msg type (car ids) "如果想要开始新的游戏请发送 “maid five new” ")])]

                           [(= (length players) 1)
                            (cond [(regexp-match #rx"join" msg)
                                   (set! players (append players (list (cadr ids))))
                                   (set! running? #t)
                                   (set! last-time (current-seconds))
                                   (init-board)
                                   (save-pic)
                                   (send-msg type (car ids) (format "[CQ:image,file=file:///tmp/pic.png][CQ:at,qq=~A]游戏开始！请输入 “maid five 橫 纵” 落子" (list-ref players 0)))]

                                  [(regexp-match #rx"new" msg)
                                   (set! players (append '() (list (cadr ids))))
                                   (set! board (make-list 225 0))
                                   (set! turn 1)
                                   (set! running? #f)
                                   (send-msg type (car ids) (format "[CQ:at,qq=~A]想要开始一局新五子棋的对决\n有意加入的请发送 “maid five join” 加入" (cadr ids)))]

                                  [else
                                   (send-msg type (car ids) "有意加入的请发送 “maid five join” 加入")])])])]

        [else #f]))


(provide five)
