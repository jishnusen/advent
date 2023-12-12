(ql:quickload :cl-ppcre)
(ql:quickload :alexandria)

(defpackage day3-part2
  (:use #:cl)
  (:export #:solve))

(in-package :day3-part2)

(defun make-pairs (l)
  (cond
    (l (cons (cons (first l) (second l))
             (make-pairs (cddr l))
             ))
    (t '())
    )
  )

(defun odd (l)
  (cond
    (l (cons (first l) (odd (cddr l))))
    (t '())
    )
  )

(defun row-matches (rs)
  (pairlis (make-pairs (ppcre:all-matches "\\d+" rs))
           (mapcar #'parse-integer (ppcre:all-matches-as-strings "\\d+" rs))
           )
  )

(defun find-stars (rs symb)
  (odd (ppcre:all-matches symb rs)))

(defun neighborhood (board row)
    (list (if (> row 0) (nth (- row 1) board))
          (nth row board)
          (nth (+ row 1) board)
          )
  )

(defun number-pair-encloses (number-pair c)
  (let ((pair (car number-pair)))
    (and (<= (car pair) (+ c 1)) (<= c (cdr pair)))
    )
  )

(defun numbers-for-star (pairs c)
  (remove-if-not (alexandria:rcurry #'number-pair-encloses c) pairs)
  )

(defun flatten-1 (list)
  (apply #'append (mapcar #'alexandria:ensure-list list)))


(defun process (board row len)
  (cond
    ((>= row len) 0)
    (t (let* ((nb (neighborhood board row))
              (numbers (flatten-1 (mapcar #'row-matches nb)))
              (stars (find-stars (nth row board) "\\*"))
              (starmap (mapcar (alexandria:curry #'numbers-for-star numbers) stars))
              (ratio-pairs (remove-if-not (lambda (l) (= (length l) 2)) starmap))
              (ratios (mapcar (lambda (p) (reduce #'* (mapcar #'cdr p))) ratio-pairs))
              )
         (+ (reduce #'+ ratios) (process board (+ row 1) len))
         )
       )
    )
  )

(defun solve (lines)
  (process lines 0 (length lines))
  )
