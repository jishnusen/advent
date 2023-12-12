(defpackage #:day1/part1
  (:use #:cl)
  (:export #:solve))

(in-package #:day1/part1)

(defun parse-number (str)
  (loop for char across str
        until (digit-char-p char)
        finally (return-from parse-number (digit-char-p char))))

(defun get-value (str)
  (+ (* (parse-number str) 10) (parse-number (reverse str))))

(defun solve (lines)
  (reduce #'+ (mapcar #'get-value lines)))
