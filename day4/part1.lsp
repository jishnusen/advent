(defun card-no (card-str)
  (parse-integer (cadr (uiop:split-string card-str))))

(defun parse-nums (lstr)
  (cond
    ((car lstr)
     (let ((n (parse-integer (car lstr) :junk-allowed t)))
       (cond
         (n (cons n (parse-nums (cdr lstr))))
         (t (parse-nums (cdr lstr))))))
    (t nil)
    )
  )

(defun calc-winnings (draw winners)
  (cond
    ((null draw) 1)
    ((member (car draw) winners) (* 2 (calc-winnings (cdr draw) winners)))
    (t (calc-winnings (cdr draw) winners)))
  )

(defun score (row)
  (let* ((dw (cadr (uiop:split-string row :separator ":")))
         (dw-split (uiop:split-string dw :separator "|"))
         (draw (parse-nums (uiop:split-string (car dw-split))))
         (winners (parse-nums (uiop:split-string (cadr dw-split))))
         )
    (floor (calc-winnings draw winners) 2)
    )
  )

(defun part1 (lines)
  (reduce #'+ (mapcar #'score lines)))
