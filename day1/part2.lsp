(defpackage day1-part2
  (:use #:cl)
  (:export #:solve))

(in-package :day1-part2)

(defparameter *wordmap* '(("one" . 1)
                          ("two" . 2)
                          ("three" . 3)
                          ("four" . 4)
                          ("five" . 5)
                          ("six" . 6)
                          ("seven" . 7)
                          ("eight" . 8)
                          ("nine" . 9)
                          ))

(defun check-word (wmap word)
  (let ((f (first wmap))
        (digit (digit-char-p (char word 0))))
    (cond
      ((null wmap) nil)
      (digit digit)
      ((uiop:string-prefix-p (car f) word) (cdr f))
      (t (check-word (rest wmap) word)))))

(defun first-num (wmap word)
  (let ((digit (check-word wmap word)))
    (cond
      ((string= word "") 0)
      (digit digit)
      (t (first-num wmap (subseq word 1))))))


(defun reverse-car (c)
  (cons (reverse (car c)) (cdr c)))

(defun get-value (str)
  (+ (* (first-num *wordmap* str) 10) (first-num (mapcar #'reverse-car *wordmap*) (reverse str))))

(defun solve (lines)
  (reduce #'+ (mapcar #'get-value lines))
  )
