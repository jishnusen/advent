(defparameter *testcase* "Game 31: 1 red, 5 blue, 10 green; 5 green, 6 blue, 12 red; 4 red, 12 blue, 4 green")

(defun split-game (str)
  (uiop:split-string str :separator ":"))

(defun split-plays (str)
  (uiop:split-string str :separator ";"))

(defun split-steps (str)
  (uiop:split-string str :separator ","))

(defun game-no (str)
 (parse-integer (car (last (uiop:split-string str)))))

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
    (let ((step (string-left-trim " " (car steps))))
      (let ((draw (parse-integer (car (uiop:split-string step))))
          (color (car (last (uiop:split-string step)))))
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

(reduce #'+ (mapcar #'check-game
          (uiop:read-file-lines "input.txt")))
