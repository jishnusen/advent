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

(defun count-matches (draw winners)
  (cond
    ((null draw) 0)
    ((member (car draw) winners) (+ 1 (count-matches (cdr draw) winners)))
    (t (count-matches (cdr draw) winners)))
  )

(defun score (row)
  (let* ((dw (cadr (uiop:split-string row :separator ":")))
         (dw-split (uiop:split-string dw :separator "|"))
         (draw (parse-nums (uiop:split-string (car dw-split))))
         (winners (parse-nums (uiop:split-string (cadr dw-split))))
         )
    (ash 1 (- (count-matches draw winners) 1))
    )
  )

(defun part1 (lines)
  (reduce #'+ (mapcar #'score lines)))
