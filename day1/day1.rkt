#! /usr/bin/env racket
#lang racket/base
(require racket/file)

(define *wordmap* (map (lambda (kv) (cons (string->list (car kv)) (cdr kv)))
                       '(("one" . 1)
                         ("two" . 2)
                         ("three" . 3)
                         ("four" . 4)
                         ("five" . 5)
                         ("six" . 6)
                         ("seven" . 7)
                         ("eight" . 8)
                         ("nine" . 9)
                         )))

(define (starts-with-digit? line)
  (let ((c (car line)))
       (and (char>=? c #\0) (char<=? c #\9))))

(define (char->digit c)
  (- (char->integer c) (char->integer #\0)))

(define (line->first-digit line)
  (cond
    ((null? line) #f)
    ((starts-with-digit? line) (char->digit (car line)))
    (#t (line->first-digit (cdr line)))
    ))

(define (line->pair-digit line)
  (+ (* (line->first-digit line) 10) (line->first-digit (reverse line))))

(define (part1 lines)
  (foldl + 0 (map line->pair-digit (map string->list lines))))


(define (prefix-match? l p)
  (cond
    ((null? p) #t)
    ((null? l) #f)
    ((equal? (car p) (car l)) (prefix-match? (cdr l) (cdr p)))
    (#t #f)
    ))

(define (line->first-match line mp)
  (let ((pref (assoc line mp prefix-match?)))
    (cond
      ((null? line) #f)
      ((starts-with-digit? line) (char->digit (car line)))
      (pref (cdr pref))
      (#t (line->first-match (cdr line) mp))
      )))

(define (line->pair-match line)
  (let ((wordmaprev (map (lambda (kv) (cons (reverse (car kv)) (cdr kv))) *wordmap*)))
    (+ (* (line->first-match line *wordmap*) 10) (line->first-match (reverse line) wordmaprev))))

(define (part2 lines)
  (foldl + 0 (map line->pair-match (map string->list lines))))

(provide main)
(define (main input)
  (let ((input (file->lines input)))
    (display (part1 input))
    (display #\newline)
    (display (part2 input))
    (display #\newline))
  )

