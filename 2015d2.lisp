;;;2015 day 2

(in-package :aoc-2015)

(defun d2-needed-area (dimension-list)
  (let ((areas (list)))
    (a:map-combinations (lambda (comb)
                          (push (apply #'* comb) areas))
                        dimension-list
                        :length 2)
    (+ (apply #'min areas) (* 2 (reduce #'+ areas)))))

(let* ((day-number 2)
       (input-name (format nil "~dd~dinput.txt" *year* day-number)))
  (defday 2
    :test-input
    '(("2x3x4"  58)
      ("1x1x10" 43))
    :parse ((mapcar (lambda (line)
                      (mapcar #'parse-integer (str:split #\x line)))
                    input))
    :p1 ((reduce #'+ (mapcar #'d2-needed-area (day-2-parse input))))
    :p1-test ((destructuring-bind (lines expected)
                  (apply #'mapcar #'list input)
                (every #'= (mapcar #'d2-needed-area (day-2-parse lines))
                       expected)))))
