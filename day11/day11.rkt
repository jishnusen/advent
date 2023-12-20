#lang racket/base
(require racket/file)
(require racket/function)
(require racket/list)
(require racket/match)

(define (find-blanks lines)
  (define (blank? line)
    (not (member #\# line)))
  (values (indexes-of (map blank? lines) #t)
          (let ([lines-t (apply map list lines)])
            (indexes-of (map blank? lines-t) #t)))
  )

(define (find-galaxies lines)
  (map cons (range (length lines)) (map (curryr indexes-of #\#) lines))
  )

(define (convert blanks-x blanks-y galaxies [scale 1])
  (define (convert-row row)
    (define (convert-y y)
      (+ y (* scale (count (curryr < y) blanks-y)))
      )
    (match row
      [(cons x ys) (cons (+ x (* scale (count (curryr < x) blanks-x)))
                         (map convert-y ys))]
      [_ row])
    )
  (map convert-row galaxies)
  )

(define (ordered-pairs galaxies)
  (define (ordered-pair-row row)
    (map (curry list (car row)) (cdr row)))
  (append* (map ordered-pair-row galaxies))
  )

(define (distance pair1 pair2)
  (foldl + 0 (map abs (map - pair1 pair2)))
  )

(define (part1 lines)
  (let ([linel (map string->list lines)])
    (let-values ([(blanks-x blanks-y) (find-blanks linel)]
                 [(galaxies) (find-galaxies linel)]
                 )
      (let ([galaxy-coords (ordered-pairs (convert blanks-x blanks-y galaxies))])
        (foldl + 0 (map (curry apply distance) (combinations galaxy-coords 2)))
        ))))

(define (part2 lines)
  (let ([linel (map string->list lines)])
    (let-values ([(blanks-x blanks-y) (find-blanks linel)]
                 [(galaxies) (find-galaxies linel)]
                 )
      (let ([galaxy-coords (ordered-pairs (convert blanks-x blanks-y galaxies 999999))])
        (foldl + 0 (map (curry apply distance) (combinations galaxy-coords 2)))
        ))))

(provide main)
(define (main input)
  (let ((input (file->lines input)))
    (display (part1 input))
    (display #\newline)
    (display (part2 input))
    (display #\newline)
    ))
