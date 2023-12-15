#!/usr/bin/env guile \
-e main -s
!#

(add-to-load-path (string-append (dirname (current-filename)) "/../lib/"))
(use-modules ((f) #:prefix f:)
             ((srfi srfi-1))
             ((ice-9 match))
             ((ice-9 regex))
             ((ice-9 curried-definitions))
             )

(define (card->list card)
  (map string->number (remove string-null? (string-split card #\space))))

(define (line->cards line)
  (let ((both (cadr (string-split line #\:))))
    (string-split both #\|)))

(define (count-matches draw winners)
  (count (lambda (n) (member n winners)) draw))

(define (line->matches line)
  (match (map card->list (line->cards line))
    ((draw winners) (count-matches draw winners))
    ))

(define (matches->score matches)
  (ash 1 (- matches 1)))

(define (part1 lines)
  (fold + 0 (map matches->score (map line->matches lines))))


(define (incr-first-n l n x)
  (match l
    ((hd . tl) (cond
                ((> n 0) (cons (+ hd x) (incr-first-n tl (- n 1) x)))
                (#t l)))
    (_ l)))

(define* (part2 lines #:optional (counts (iota (length lines) 1 0)))
  (match lines
    ((hd . tl) (+ (car counts)
                  (part2 tl (incr-first-n (cdr counts) (line->matches hd) (car counts)))))
    (_ 0))
  )


(define (main argv)
  (let ((input (f:read-lines (cadr argv))))
    (display (part1 input))
    (display #\newline)
    (display (part2 input))
    (display #\newline))
  )
