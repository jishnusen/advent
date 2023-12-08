(defun parse-number (str)
  (loop for char across str
        until (digit-char-p char)
        finally (return-from parse-number (digit-char-p char))))

(defun get-value (str)
  (+ (* (parse-number str) 10) (parse-number (reverse str))))

(princ
  (reduce #'+ (mapcar #'get-value
                      (uiop:read-file-lines "/home/jishnu/Documents/code/advent-clisp/day1/input.txt"))))

