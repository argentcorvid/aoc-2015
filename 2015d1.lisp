;;;2015 day 1

(in-package :aoc-2015)

(defparameter *day-number* 1)
(defparameter *input-name* (format nil "~dd~dinput.txt" *year* *day-number*))

(defun d1-conv (char)
  (case char
    (#\( 1)
    (#\) -1)))

(defday 1
  :test-input
  '(("(())" . 0)
    ("()()" . 0)
    ("(((" . 3)
    ("(()(()(" . 3)
    ("))(((((" . 3)
    ("())" . -1)
    ("))(" . -1)
    (")))" . -3)
    (")())())" . -3))
  :p1 ((reduce #'+ input :key #'d1-conv))
  :p1-test ((every (lambda (pair)
                     (= (day-1-p1 (car pair)) (cdr pair)))
                   input))
  :p2 ((loop :for idx :from 1
             :for step :across input
             :summing (d1-conv step) :into floor
             :when (minusp floor)
               :return idx)))
