;;;; advent-clisp.asd

(asdf:defsystem #:advent-clisp
  :description "Advent of Code 2023 Solutions"
  :author "Jishnu Sen <jishnu1@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-ppcre #:alexandria)
  :components ((:file "day1/part1")
               (:file "day1/part2")
               (:file "day2/day2")
               (:file "day3/part1")
               (:file "day3/part2")
               (:file "day4/day4")
               (:file "main")
               )
  :build-operation "program-op"
  :build-pathname "advent"
  :entry-point "main:main"
  )
