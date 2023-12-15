#!/usr/bin/env guile \
-e main -s
!#

(add-to-load-path (string-append (dirname (current-filename)) "/../lib/"))
(use-modules ((f) #:prefix f:)
             ((srfi srfi-1))
             ((ice-9 match))
             ((ice-9 curried-definitions))
             )

(define (game-id line)
  (string->number (cadr (string-split (car (string-split line #\:)) #\space))))

(define (read-game line)
  (concatenate (map (lambda (s) (string-split s #\,))
                    (string-split (cadr (string-split line #\:)) #\;))))

(define (read-draw draw)
  (let ((stripped-draw (remove string-null? (string-split draw #\space))))
    (match stripped-draw
      ((n c) (list (string->number n) c)))))

(define (check-draw pair)
  (let ((n (car pair))
        (c (cadr pair)))
    (match c
      ("red" (<= n 12))
      ("green" (<= n 13))
      ("blue" (<= n 14))
      )
    ))

(define (game-valid? line)
  (not (member #f (map check-draw (map read-draw (read-game line))))))

(define (part1 lines)
  (fold + 0 (map game-id (filter game-valid? lines))))

(define ((max-color draws) c)
  (fold max 1 (map car (filter (lambda (draw) (string=? c (cadr draw))) draws))))

(define (power draws)
  (fold * 1 (map (max-color draws) '("red" "green" "blue"))))

(define (line->power line)
  (power (map read-draw (read-game line))))

(define (part2 lines)
  (fold + 0 (map line->power lines)))

(define (main argv)
  (let ((input (f:read-lines (cadr argv))))
    (display (part1 input))
    (display #\newline)
    (display (part2 input))
    (display #\newline))
  )
