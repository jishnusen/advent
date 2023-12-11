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

(defun count-row-matches (row)
  (let* ((dw (cadr (uiop:split-string row :separator ":")))
         (dw-split (uiop:split-string dw :separator "|"))
         (draw (parse-nums (uiop:split-string (car dw-split))))
         (winners (parse-nums (uiop:split-string (cadr dw-split))))
         )
    (count-matches draw winners)
    )
  )

(defun score (row)
    (ash 1 (- (count-row-matches row) 1))
  )

(defun part1 (lines)
  (reduce #'+ (mapcar #'score lines)))

(defun incr-list (l n a)
  (cond
    ((> n 0) (cons (+ (car l) a) (incr-list (cdr l) (- n 1) a)))
    (t l))
  )

(defun count-cards (lines counts)
  (cond
    ((null lines) 0)
    (t (let* ((row (car lines))
             (matches (count-row-matches row))
             (our-count (car counts))
             )
        (+ our-count (count-cards (cdr lines) (incr-list (cdr counts) matches our-count))))
       )
    )
  )

(defun part2 (lines)
  (count-cards lines (make-list (length lines) :initial-element 1))
  )
