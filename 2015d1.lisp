;;;2015 day 1

(in-package :aoc-2015)

(defun d1-conv (char)
  (case char
    (#\( 1)
    (#\) -1)))

(defday 1
  :test-input
  '("(())"
    "()()"
    "((("
    "(()(()("
    "))((((("
    "())"
    "))("
    ")))"
    ")())())")
  :p1-test-expected '(0 0 3 3 3 -1 -1 -3 -3)
  :p1 ((reduce #'+ input :key #'d1-conv))
  :p1-test ((every (lambda (line exp)
                     (= (day-1-p1 line) exp))
                   %test-input% %p1-expect%))
  :p2 ((loop :for idx :from 1
             :for step :across input
             :summing (d1-conv step) :into floor
             :when (minusp floor)
               :return idx)))
