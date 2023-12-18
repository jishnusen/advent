#lang racket/base
(require racket/file)
(require racket/match)
(require racket/string)
(require srfi/1)

(define (card->list card)
  (map string->number (string-split card)))

(define (line->cards line)
  (let ((both (cadr (string-split line ":"))))
    (string-split both "|")))

(define (count-matches draw winners)
  (count (lambda (n) (member n winners)) draw))

(define (line->matches line)
  (match (map card->list (line->cards line))
    [(list draw winners) (count-matches draw winners)]
    ))

(define (matches->score matches)
  (arithmetic-shift 1 (- matches 1)))

(define (part1 lines)
  (foldl + 0 (map matches->score (map line->matches lines))))


(define (incr-first-n l n x)
  (match l
    [(cons hd tl) (cond
                   ((> n 0) (cons (+ hd x) (incr-first-n tl (- n 1) x)))
                   (#t l))]
    [_ l]))

(define (part2 lines [counts (iota (length lines) 1 0)])
  (match lines
    [(cons hd tl) (+ (car counts)
                     (part2 tl
                            (incr-first-n (cdr counts) (line->matches hd) (car counts))))]
    [_ 0])
  )

(provide main)
(define (main input)
  (let ((input (file->lines input)))
    (display (part1 input))
    (display #\newline)
    (display (part2 input))
    (display #\newline))
  )
