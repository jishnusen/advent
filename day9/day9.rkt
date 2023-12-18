#lang racket/base
(require racket/file)
(require racket/function)
(require racket/list)
(require racket/string)
(require (only-in srfi/1 map))

(define (parse lines)
  (map (curry map string->number) (map string-split lines)))

(define (predict pos foldt op in)
  (define (get-diffs l)
    (let ([diff (map - (cdr l) l)])
        (cond
        [(andmap zero? l) '()]
        [#t (cons (pos l) (get-diffs diff))]
        )))
  (foldt op 0 (get-diffs in))
  )

(define (part1 lines)
  (foldl + 0 (map (curry predict last foldl +) (parse lines)))
  )

(define (part2 lines)
  (foldl + 0 (map (curry predict first foldr -) (parse lines)))
  )

(provide main)
(define (main input)
  (let ((input (file->lines input)))
    (display (part1 input))
    (display #\newline)
    (display (part2 input))
    (display #\newline)
    ))
