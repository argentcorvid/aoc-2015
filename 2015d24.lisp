;;;2015 day 24

(in-package :aoc-2015)

(defun quantum-entanglement (&rest numbers)
  (apply #'* numbers))

(defun greater-legroom-p (pkg-list-a pkg-list-b)
  (let ((len-a (length pkg-list-a))
        (len-b (length pkg-list-b)))
    (or (< len-a len-b)
        (and (= len-a len-b)
             (< (apply #'quantum-entanglement pkg-list-a)
                (apply #'quantum-entanglement pkg-list-b))))))

(defday 24
  :test-input
  (str:lines
   "1
2
3
4
5
7
8
9
10
11")
  :parse ((mapcar #'parse-integer input))
  :p1 ((loop :with heap := (s:make-heap :test #'greater-legroom-p)
             :for n :from 2 :upto (- (length input) 2)
             :do (a:map-combinations
                  (lambda (comb &aux (others (set-difference input comb)))
                    (when (= (apply #'+ comb)
                             (/ (apply #'+ others) 2))
                      (s:heap-insert heap comb)))
                  input :length n)
             :finally (let ((best (s:heap-extract-maximum heap)))
                        (return (values best
                                        (apply #'quantum-entanglement best))))))
  :p1-test-expected '((11 9) 99)
  :p1-test ((day-24-p1 (day-24-parse %test-input%)))
  :p2 ())
