(defparameter *testcase* "3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")

(defun strip-split (str &key separator)
  (remove ""
          (mapcar (lambda (s) (string-left-trim " " s))
                  (uiop:split-string str :separator (or separator " "))
                  )
          :test #'equal
          )
  )

(defun split-game (str)
  (strip-split str :separator ":"))

(defun split-plays (str)
  (strip-split str :separator ";"))

(defun split-steps (str)
  (strip-split str :separator ","))

(defun game-no (str)
 (parse-integer (car (last (strip-split str)))))

(defun check-draw (draw color)
  (cond
    ((string= color "red") (<= draw 12))
    ((string= color "green") (<= draw 13))
    ((string= color "blue") (<= draw 14))
    (t nil)
    )
  )

(defun validate (steps)
  (if steps
    (let ((step (car steps)))
      (let ((draw (parse-integer (car (strip-split step))))
          (color (car (last (strip-split step)))))
        (cond
          ((check-draw draw color) (validate (cdr steps)))
          (t nil)
          )
        )
      )
    t
  ))

(defun check-plays (plays)
  (cond
    ((null plays) t)
    ((validate (split-steps (car plays))) (check-plays (cdr plays)))
    (t nil)
    ))

(defun check-game (game)
  (let ((gs (split-game game)))
    (let ((game (game-no (car gs)))
      (plays (split-plays (car (last gs)))))
      (cond
        ((check-plays plays) game)
        (t 0)
        )
      )
    )
  )

(defun part1 ()
  (reduce #'+
          (mapcar #'check-game (uiop:read-file-lines "input.txt"))
          )
  )
