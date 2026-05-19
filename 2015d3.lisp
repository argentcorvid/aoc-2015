;;;2015 day 3

(in-package :aoc-2015)

(defday 3
  :test-input
  '(">"
    "^>v<"
    "^v^v^v^v^v")
  :p1-test-expected
  '(2 4 2)
  :p2-test-expected
  '(3 3 11)
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

  :p1-test ((every #'=
                   (mapcar #'day-3-p1 (mapcar #'day-3-parse %test-input%))
                   %p1-expect%))

  :p2 ((do* ((santa-pos 0 (+ santa-pos (pop input)))
             (robo-pos 0 (+ robo-pos (pop input)))
             (santa-seen (list 0) (pushnew santa-pos santa-seen :test #'=))
             (robo-seen (list 0) (pushnew robo-pos robo-seen :test #'=)))
            ((null input) (length (union santa-seen robo-seen :test #'=)))))

  :p2-test ((let ((%test-input% (replace (copy-list %test-input%) '("^v"))))
              (every #'=
                     (mapcar #'day-3-p2 (mapcar #'day-3-parse %test-input%))
                     %p2-expect%))))
