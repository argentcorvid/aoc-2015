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
  '(("2x3x4"  58 34)
    ("1x1x10" 43 14))
  :parse ((mapcar (lambda (line)
                    (mapcar #'parse-integer (str:split #\x line)))
                  input))
  :p1 ((reduce #'+ (mapcar #'d2-needed-area (day-2-parse input))))
  :p1-test ((destructuring-bind (lines expected ign)
                (apply #'mapcar #'list input)
              (declare (ignore ign))
              (every #'= (mapcar #'d2-needed-area (day-2-parse lines))
                     expected)))
  :p2 ((reduce #'+ (mapcar #'d2-needed-ribbon (day-2-parse input))))
  :p2-test ((destructuring-bind (lines ign expected)
                (apply #'mapcar #'list input)
              (declare (ignore ign))
              (every #'= (mapcar #'d2-needed-ribbon (day-2-parse lines))
                     expected))))

