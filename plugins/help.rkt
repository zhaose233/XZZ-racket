#lang racket

(require "../lib.rkt")

(define (help type ids msg)
  (send-msg type (car ids)
            (string-append "妾身可以为您提供以下服务：\n"
                           "1. help 查看此列表\n"
                           "2. hello 跟女仆酱打声招乎\n"
                           "3. echo [内容] 让女仆重复你的话\n"
                           "4. five new 新开一局五子棋\n"

                           "**请在所有命令前都加上maid！**"
                           )))

(provide help)
