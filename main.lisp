(defpackage #:main
  (:use #:cl)
  (:export #:main))

(in-package #:main)

(defparameter *funs* (list
                       #'day1/part1:solve
                       #'day1/part2:solve
                       #'day2:part1
                       #'day2:part2
                       #'day3/part1:solve
                       #'day3/part2:solve
                       #'day4:part1
                       #'day4:part2
                       ))

(defun run (d f)
  (let ((input (uiop:read-file-lines f))
        (fmap (pairlis '(("day1" . "part1")
                         ("day1" . "part2")
                         ("day2" . "part1")
                         ("day2" . "part2")
                         ("day3" . "part1")
                         ("day3" . "part2")
                         ("day4" . "part1")
                         ("day4" . "part2"))
                        *funs*))
        )
    (princ (funcall (cdr (assoc d fmap :test #'equal)) input))
    (terpri)
    )
  )

(defun main ()
  (let ((args (uiop:command-line-arguments)))
    (cond
      ((= 3 (length args)) (run (cons (first args) (second args)) (third args)))
      (t (write-string "usage: DAY PART INPUT"))
      ))
  )
