(defun lastcar (l) (car (last l)))

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
 (parse-integer (lastcar (strip-split str))))

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
          (color (lastcar (strip-split step))))
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
          (plays (split-plays (lastcar gs))))
      (cond
        ((check-plays plays) game)
        (t 0)
        )
      )
    )
  )

(defun part1 (lines)
  (reduce #'+ (mapcar #'check-game lines)))

(defun max-color (c steps)
  (let ((step (strip-split (car steps))))
    (let ((draw (car step))
          (color (lastcar step)))
      (cond
        ((null step) 1)
        ((string= color c) (max (parse-integer draw) (max-color c (cdr steps))))
        (t (max-color c (cdr steps)))
        )
      )
    )
  )

(defun max-color-game (c game)
  (let ((plays (lastcar (split-game game))))
    (reduce #'max (mapcar (lambda (p) (max-color c (split-steps p))) (split-plays plays)))
    )
  )

(defun power-game (game)
  (reduce #'* (mapcar (lambda (c) (max-color-game c game)) '("red" "green" "blue")))
  )

(defun part2 (lines)
  (reduce #'+ (mapcar #'power-game lines)))
