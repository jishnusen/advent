(defpackage #:day5
  (:import-from :trivia #:match #:guard)
  (:use #:cl)
  (:export #:part1))

(in-package #:day5)

(defun parse-map-title (line)
  (let ((sp (ppcre:split "-to-| map" line)))
    (match sp
      ((list k v _) (cons k v)))
    )
  )

(defun parse-dict-row (line)
  (let ((sp (uiop:split-string line)))
    (mapcar #'parse-integer sp)
    )
  )

(defun parse-dict (lines)
  (let ((l (car lines)))
    (cond
      ((or (null l) (string= l "")) '())
      (t (cons (parse-dict-row l) (parse-dict (cdr lines))))
      )
    ))

(defun build-map (lines)
  (let ((title (parse-map-title (car lines))))
      (cond
        ((null lines) '())
        (title (cons (cons title (parse-dict (cdr lines))) (build-map (cdr lines))))
        (t (build-map (cdr lines)))
      )
    )
  )

(defun lookup (k m)
  (match (car m)
    (nil k)
    ((guard (list r s e)
            (and (>= k s) (< k (+ s e)))) (+ r (- k s)))
    ((list* _) (lookup k (cdr m)))
    )
  )

(defun find-seed (k mn f)
  (let* ((entry (assoc mn f :key #'car :test #'string=))
         (next (assoc (cdr (car entry)) f :key #'car :test #'string=))
         (val (lookup k (cdr entry))))
    (cond
      (next (find-seed val (car (car next)) f))
      (t val)
    )))

(defun part1 (lines)
  (let ((seeds (mapcar #'parse-integer (cdr (uiop:split-string (car lines)))))
        (seedmap (build-map lines)))
    (reduce #'min (mapcar (alexandria:rcurry #'find-seed "seed" seedmap) seeds))
    )
  )
