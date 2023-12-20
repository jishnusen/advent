#lang racket/base
(require racket/file)
(require racket/function)
(require racket/list)
(require racket/match)
(require racket/set)
(require racket/string)
(require racket/hash)

(define (rel-coord board cur off)
  (let ([nrow (+ (car cur) (car off))]
        [ncol (+ (cdr cur) (cdr off))])
    (if (and (and (>= nrow 0) (< nrow (vector-length board)))
             (and (>= ncol 0) (< ncol (string-length (vector-ref board 0)))))
        (string-ref (vector-ref board nrow) ncol)
        #f
        )
    ))

(define *dirs* '((0 . 1) (1 . 0) (0 . -1) (-1 . 0)))
(define *pipe-to-dirs*
  #hash(
        [#\| . ((1 . 0) (-1 . 0))]
        [#\- . ((0 . 1) (0 . -1))]
        [#\L . ((1 . 0) (0 . -1))]
        [#\J . ((0 . 1) (1 . 0))]
        [#\7 . ((0 . 1) (-1 . 0))]
        [#\F . ((0 . -1) (-1 . 0))]
        )
  )
(define *dirs-to-pipe*
  (make-hash (hash-map *pipe-to-dirs* (lambda (k v) (cons v k)))))

(define *dir-to-pipes*
  (let ([pipes (hash->list *pipe-to-dirs*)])
    (let ([k (map car pipes)])
      (let* ([gates-for-dir (lambda (acc d e) (equal? d (acc (hash-ref *pipe-to-dirs* e))))]
             [zip-acc (lambda (acc v) (cons v acc))]
             [acc-gates-for-dir (lambda (acc d) (map (curry zip-acc acc) (filter (curry gates-for-dir acc d) k)))]
             [acc->ht (lambda (acc) (make-hash (map cons *dirs* (map (curry acc-gates-for-dir acc) *dirs*))))]
             [left (acc->ht car)]
             [right (acc->ht cadr)]
             )
        (hash-union! left right
                     #:combine/key (lambda (k v1 v2) (append v1 v2)))
        left
        )
    )))

(define (exit e)
  (cond
    [(equal? e car) cadr]
    [#t car])
  )

(define (add-c a b)
  (cons (+ (car a) (car b)) (+ (cdr a) (cdr b))))

(define (flip c)
  (cons (- (car c)) (- (cdr c))))

(define (find-init board)
  (let* ([row (index-where (vector->list board) (curryr string-contains? "S"))]
         [col (index-of (string->list (vector-ref board row)) #\S)]
         [coord (cons row col)]
         [dirs (filter (lambda (d) (assoc (rel-coord board coord d) (hash-ref *dir-to-pipes* d))) *dirs*)])
    (values coord dirs)
    ))

(define (get-loop board cur-pos dir)
  (let* ([next (rel-coord board cur-pos dir)]
         [pipe (assoc next (hash-ref *dir-to-pipes* dir))]
         [newdir (if pipe (flip ((exit (cdr pipe)) (hash-ref *pipe-to-dirs* (car pipe)))) #f)])
    (cond
     ((char=? next #\S) (list cur-pos))
     (#t (cons cur-pos (get-loop board (add-c cur-pos dir) newdir)))
     )))

(define (part1 lines)
  (let ([board (list->vector lines)])
    (let-values ([(start dirs) (find-init board)])
      (quotient (length (get-loop board start (car dirs))) 2)
      )))

(define *walls* '[#\| #\L #\J])

(define (char-at board row col)
  (string-ref (vector-ref board row) col)
  )

(define (get-pair-distances board loop row [col 0] [in #f])
  (cond
   [(= col (string-length (vector-ref board row))) 0]
   [(set-member? loop (cons row col))
    (cond
     [(set-member? *walls* (char-at board row col)) (get-pair-distances board loop row (+ col 1) (not in))]
     [#t (get-pair-distances board loop row (+ col 1) in)]
     )]
   [in (+ 1 (get-pair-distances board loop row (+ 1 col) in))]
   [#t (get-pair-distances board loop row (+ col 1) in)]
   ))

(define (part2 lines)
  (let ([board (list->vector lines)])
    (let-values ([(start dirs) (find-init board)])
      (let* ([s-dirs (map flip dirs)]
             [s-pipe (or (hash-ref *dirs-to-pipe* s-dirs #f) (hash-ref *dirs-to-pipe* (reverse s-dirs)))]
             [loop (list->set (get-loop board start (car dirs)))])
        (string-set! (vector-ref board (car start)) (cdr start) s-pipe)
        (foldl + 0 (map (curry get-pair-distances board loop) (range (vector-length board))))
        )
      )))

(provide main)
(define (main input)
  (let ((input (file->lines input)))
    (display (part1 input))
    (display #\newline)
    (display (part2 input))
    (display #\newline)
    ))
