(load "part1.lsp")

(defun lastcar (l) (car (last l)))

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

(defun part2 ()
  (reduce #'+ (mapcar #'power-game
                      (uiop:read-file-lines "input.txt")
                      )
          )
  )
