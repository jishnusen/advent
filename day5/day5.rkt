#lang racket/base
(require racket/match)
(require racket/string)
(require racket/file)
(require srfi/8)
(require srfi/1)

(define (title->mapping title)
  (let ((delim (car (regexp-match-positions #rx"-to-" title))))
    (cons (substring title 0 (car delim))
          (substring title (cdr delim)))
    ))

(define (sort-ranges k range)
  (values k (sort range (lambda (x y) (< (second x) (second y)))))
  )

(define (parse lines [cur-key #f] [key-table (hash)] [range-table (hash)] [seeds '()])
  (match lines
    [(cons hd tl) (let ((split (string-split hd))
                        (cur-range (hash-ref range-table cur-key '())))
                 (match split
                   [(cons "seeds:" s) (parse tl cur-key key-table range-table
                                             (map string->number s))]
                   [(list title "map:") (match (title->mapping title)
                                          [(cons k v) (parse tl k
                                                             (hash-set key-table k v)
                                                             range-table seeds)]
                                          )]
                   [(list _ _ _) (parse tl cur-key key-table
                                   (hash-set range-table cur-key
                                             (cons (map string->number split) cur-range))
                                   seeds)]
                   [_ (parse tl cur-key key-table range-table seeds)]
                   ))
     ]
    [_ (values key-table (hash-map/copy range-table sort-ranges) seeds)]
    ))

(define (in-range? entry k)
  (and (>= k (second entry)) (< k (+ (second entry) (third entry)))))

(define (key->val m k l)
  (cond
   ((null? m) (values k 0))
   ((in-range? (car m) k) (let ((val (+ (- k (second (car m))) (first (car m))))
                                (cov (- (+ (second (car m)) (third (car m))) k)))
                            (cond
                             ((< cov l) (values val (- l cov)))
                             (#t (values val 0))
                             )
                            ))
   ((< k (second (car m))) (let ((next (second (car m))))
                             (cond
                              ((< next (+ k l)) (values k (- (+ k l) next)))
                              (#t (values k 0))
                              )
                             ))
   (#t (key->val (cdr m) k l))
  ))

(define ((seed->location key-table range-table m) key)
  (let ((next (hash-ref key-table m #f))
        (ranges (hash-ref range-table m #f)))
      (cond
       (ranges (receive (n _) (key->val ranges key 0)
                 ((seed->location key-table range-table next) n)))
       (#t key))))

(define (part1 lines)
  (receive (key-table range-table seeds) (parse lines)
    (reduce min -1 (map (seed->location key-table range-table "seed") seeds))
    ))

(define ((trace-range m) k l)
  (receive (v r) (key->val m k l)
    (let ((covered (- l r)))
      (cond
       ((= r 0) (list (cons v l)))
       (#t (cons (cons v covered) ((trace-range m) (+ k covered) r)))
       )
      )
  ))

(define (trace-through key-table range-table cur-map inrange)
  (let ((next (hash-ref key-table cur-map #f))
        (ranges (hash-ref range-table cur-map #f)))
      (cond
       (ranges (trace-through key-table range-table next
        (concatenate (map (trace-range ranges) (map car inrange) (map cdr inrange)))))
       (#t inrange)))
  )

(define (seeds->ranges l)
  (cond
   ((null? l) '())
   (#t (cons (cons (car l) (cadr l)) (seeds->ranges (cddr l))))
   ))

(define (part2 lines)
  (receive (key-table range-table seeds) (parse lines)
    (reduce min -1 (map car (trace-through key-table range-table "seed" (seeds->ranges seeds))))
    )
  )

(provide main)
(define (main input)
  (let ((input (file->lines input)))
    (display (part1 input))
    (display #\newline)
    (display (part2 input))
    (display #\newline)
    )
  )
