#lang racket/base
(require racket/file)
(require racket/function)
(require racket/match)
(require racket/generator)

(define (parse lines)
  (define (dir->fn dir)
    (case dir
      [(#\L) car]
      [(#\R) cdr]
      ))
  (define (parse-dict rem [h (hash)])
    (cond
     ((null? rem) h)
     (#t (match (regexp-match* "[0-9A-Z]+" (car rem))
           [(list k l r) (parse-dict (cdr rem) (hash-set h k (cons l r)))]
           ))
     )
    )
  (values (map dir->fn (string->list (car lines))) (parse-dict (cddr lines)))
)

(define (traverse lr ht)
  (define next-dir (infinite-generator (for-each yield lr)))
  (define (traverse-helper ht cur)
    (let ([entry (hash-ref ht cur)])
      (cond
       ((string=? cur "ZZZ") 0)
       (#t (+ 1 (traverse-helper ht ((next-dir) entry))))
       )))
  (traverse-helper ht "AAA")
  )

(define (part1 lines)
  (call-with-values (lambda () (parse lines)) traverse))


(define (traverse-mult lr ht)
  (define (start-key? k) (char=? (string-ref k 2) #\A))
  (define (end-key? k) (char=? (string-ref k 2) #\Z))

  (define (traverse-helper ht cur [next-dir (sequence->repeated-generator lr)])
    (let ([entry (hash-ref ht cur)]
          [next-fn (next-dir)])
      (cond
       ((end-key? cur) 0)
       (#t (+ 1 (traverse-helper ht (next-fn entry) next-dir)))
       )))
  (apply lcm (map (curry traverse-helper ht) (filter start-key? (hash-keys ht))))
  )

(define (part2 lines)
  (call-with-values (lambda () (parse lines)) traverse-mult))


(provide main)
(define (main input)
  (let ((input (file->lines input)))
    (display (part1 input))
    (display #\newline)
    (display (part2 input))
    (display #\newline)
    ))
