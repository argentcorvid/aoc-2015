;;;2015 day 14

(in-package :aoc-2015)

(defun reindeer-distance (reindeer time)
  (destructuring-bind (name rate active rest)
      reindeer
    (multiple-value-bind (periods rem)
        (floor time (+ active rest))
      (list name (+ (* rate periods active)
                    ())))))

(defday 14
  :test-input
  "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."
  :parse ((let (data)
            (dolist (l (s:lines input))
              (push (mapcar (lambda (itm)
                              (if (find-if #'digit-char-p itm)
                                  (parse-integer itm)
                                  itm))
                            (pick-elements (str:words l) 0 3 6 13))
                    data))
            data))
  :p1 ((let ((time (or (first more) 0))
             distances)
         (dolist (reindeer input)
           (push (reindeer-distance reindeer time)
                 distances))
         (s:extrema distances #'> :key #'second)))
  :p1-test ((equal (princ (day-14-p1 (day-14-parse %test-input%) 1000)) %p1-expect% ))
  :p1-test-expected '("Comet" 1120)
  :p2 ())
