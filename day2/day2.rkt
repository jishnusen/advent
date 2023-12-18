#lang racket/base
(require racket/file)
(require racket/string)
(require racket/match)
(require srfi/1)

(define (game-id line)
  (string->number (cadr (string-split (car (string-split line ":"))))))

(define (read-game line)
  (concatenate (map (lambda (s) (string-split s ","))
                    (string-split (cadr (string-split line ":")) ";"))))

(define (read-draw draw)
  (let ((stripped-draw (string-split draw)))
    (match stripped-draw
      [(list n c) (list (string->number n) c)]
      )))

(define (check-draw pair)
  (let ((n (car pair))
        (c (cadr pair)))
    (match c
      ["red" (<= n 12)]
      ["green" (<= n 13)]
      ["blue" (<= n 14)]
      )
    ))

(define (game-valid? line)
  (not (member #f (map check-draw (map read-draw (read-game line))))))

(define (part1 lines)
  (foldl + 0 (map game-id (filter game-valid? lines))))

(define ((max-color draws) c)
  (foldl max 1 (map car (filter (lambda (draw) (string=? c (cadr draw))) draws))))

(define (power draws)
  (foldl * 1 (map (max-color draws) '("red" "green" "blue"))))

(define (line->power line)
  (power (map read-draw (read-game line))))

(define (part2 lines)
  (foldl + 0 (map line->power lines)))

(provide main)
(define (main input)
  (let ((input (file->lines input)))
    (display (part1 input))
    (display #\newline)
    (display (part2 input))
    (display #\newline))
  )
