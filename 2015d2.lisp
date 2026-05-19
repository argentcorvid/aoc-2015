;;;2015 day 2

(in-package :aoc-2015)

(defun d2-needed-area (dimension-list)
  (let ((areas (list)))
    (a:map-combinations (lambda (comb)
                          (push (apply #'* comb) areas))
                        dimension-list
                        :length 2)
    (+ (apply #'min areas) (* 2 (reduce #'+ areas)))))

(defun d2-needed-ribbon (dimension-list)
  (let* ((smallest-face (subseq (sort (copy-list dimension-list) #'<) 0 2))
         (volume (apply #'* dimension-list)))
    (+ volume (* 2 (apply #'+ smallest-face)))))

(defday 2
  :test-input
  '("2x3x4"
    "1x1x10")
  :p1-test-expected '(58
                      34)
  :p2-test-expected '(43
                      14)
  :parse ((mapcar (lambda (line)
                    (mapcar #'parse-integer (str:split #\x line)))
                  input))
  :p1 ((reduce #'+ (mapcar #'d2-needed-area (day-2-parse input))))
  :p1-test ((every #'= (mapcar #'d2-needed-area (day-2-parse %test-input%))
                   %p1-expect%))
  :p2 ((reduce #'+ (mapcar #'d2-needed-ribbon (day-2-parse input))))
  :p2-test ((every #'= (mapcar #'d2-needed-ribbon (day-2-parse %test-input%))
                   %p2-expect%)))

