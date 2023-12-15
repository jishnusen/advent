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

(define (match-numbers line)
  (list-matches "[0-9]+" line))

(define (neighborhood board row)
  (let ((l (length board)))
    (list (list-ref board (max 0 (- row 1)))
          (list-ref board row)
          (list-ref board (min (+ row 1) (- l 1))))
  ))

(define (match->pair m)
  (cons (match:start m) (match:end m)))

(define ((match-near? m) x)
  (match (match->pair m)
    ((a . b) (and (<= (- a 1) x) (<= x b)))))

(define ((sum-row-valid board) row)
  (let ((nb (neighborhood board row))
        (nums (match-numbers (list-ref board row))))
    (let* ((syms (map match:start
                      (concatenate (map (lambda (r) (list-matches "[^\\.0-9]" r))
                                        nb))))
           (num-valid (filter (lambda (p) (member #t (map (match-near? p) syms))) nums))
           (num-ints (map string->number (map match:substring num-valid))))
      (fold + 0 num-ints)
      )
    ))

(define (part1 lines)
  (fold + 0 (map (sum-row-valid lines) (iota (length lines)))))


(define ((sym-ratio-sum board) row)
  (let ((nb (neighborhood board row))
        (stars (map match:start (list-matches "\\*" (list-ref board row)))))
     (let* ((numbers (concatenate (map match-numbers nb)))
            (match-near-rcurry (lambda (x) (lambda (m) ((match-near? m) x))))
            (numbers-for-star (lambda (star) (filter (match-near-rcurry star) numbers)))
            (starmap (map numbers-for-star stars))
            (two? (lambda (l) (= 2 (length l))))
            (good-stars (filter two? starmap))
            (ratios (map (lambda (p) (fold * 1 (map string->number
                                                    (map match:substring p)))) good-stars))
            )
       (fold + 0 ratios)
       )
    ))

(define (part2 lines)
  (fold + 0 (map (sym-ratio-sum lines) (iota (length lines)))))

(define (main argv)
  (let ((input (f:read-lines (cadr argv))))
    (display (part1 input))
    (display #\newline)
    (display (part2 input))
    (display #\newline))
  )
