;;;2015 day 3

(in-package :aoc-2015)

(defday 3
  :test-input
  '(">"
    "^>v<"
    "^v^v^v^v^v")
  :p1-test-expected
  '(2 4 2)
  :parse ((map 'list
               (lambda (char)
                 (case char
                   (#\> #C(1 0))
                   (#\< #c (-1 0))
                   (#\^ #c (0 1))
                   (#\v #c (0 -1))))
               input))
  :p1 ((do* ((pos 0 (+ pos (pop input)))
             (seen (list 0) (pushnew pos seen :test #'=)))
            ((null input) (length seen))))
  :p1-test ((every #'= (mapcar #'day-3-p1 (mapcar #'day-3-parse %test-input%)) %p1-expect%)))
