#lang racket/base
(require racket/file)
(require racket/function)
(require racket/list)
(require racket/string)

(define (rel-coord board cur off)
  (let ([nrow (+ (car cur) (car off))]
        [ncol (+ (cdr cur) (cdr off))])
    (if (and (and (>= nrow 0) (< nrow (vector-length board)))
             (and (>= ncol 0) (< ncol (string-length (vector-ref board 0)))))
        (string-ref (vector-ref board nrow) ncol)
        #f
        )
    ))

(define *pipe-full*
  #hash(
        [r .
           #hash(
                 [#\| . (-1 . 0)]
                 [#\- . (0 . -1)]
                 [#\L . (0 . -1)]
                 [#\J . (0 . 1)]
                 [#\7 . (0 . 1)]
                 [#\F . (0 . -1)]
                 )
           ]
        [l .
           #hash(
                 [#\| . (1 . 0)]
                 [#\- . (0 . 1)]
                 [#\L . (1 . 0)]
                 [#\J . (1 . 0)]
                 [#\7 . (-1 . 0)]
                 [#\F . (-1 . 0)]
                 )
           ]
        )
  )

(define *dirs* '((0 . 1) (1 . 0) (0 . -1) (-1 . 0)))
(define *dir-to-pipes*
  (let* ([entries (append* (map
                            (lambda (s) (hash-map (hash-ref *pipe-full* s)
                                                  (lambda (k v) (cons v (cons k s)))))
                            (hash-keys *pipe-full*)))]
         [keys (remove-duplicates (map car entries))])
    (make-hash (map cons keys
                    (map (lambda (k) (map cdr
                                          (filter (lambda (e) (equal? k (car e)))
                                                  entries)))
                         keys)))
    )
  )

(define (exit e)
  (case e
    [(r) 'l]
    [(l) 'r])
  )

(define (add-c a b)
  (cons (+ (car a) (car b)) (+ (cdr a) (cdr b))))

(define (flip c)
  (cons (- (car c)) (- (cdr c))))

(define (find-init board)
  (let* ([row (index-where (vector->list board) (curryr string-contains? "S"))]
         [col (index-of (string->list (vector-ref board row)) #\S)]
         [coord (cons row col)]
         [dir (findf (lambda (d) (assoc (rel-coord board coord d) (hash-ref *dir-to-pipes* d))) *dirs*)])
    (values coord dir)
    )
  )

(define (traverse board cur-pos dir)
  (let* ([next (rel-coord board cur-pos dir)]
         [pipe (assoc next (hash-ref *dir-to-pipes* dir))]
         [newdir (if pipe (flip (hash-ref (hash-ref *pipe-full* (exit (cdr pipe))) (car pipe))) #f)])
    (cond
     ((char=? next #\S) 1)
     (#t (+ 1 (traverse board (add-c cur-pos dir) newdir)))
     )
    )
  )

(define (part1 lines)
  (let ([board (list->vector lines)])
    (let-values ([(start dir) (find-init board)])
      (quotient (traverse board start dir) 2)
      )))


(provide main)
(define (main input)
  (let ((input (file->lines input)))
    (display (part1 input))
    (display #\newline)
    ))
