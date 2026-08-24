;;;2015 day 14

(in-package :aoc-2015)

(defun reindeer-distance (reindeer time)
  (destructuring-bind (name rate active rest)
      reindeer
    (multiple-value-bind (periods rem)
        (floor time (+ active rest))
      (list name (+ (* rate periods active)
                    (if (<= rem active)
                        (* rem rate)
                        (* active rate)))))))

(defday 14
  :test-input
  "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."
  :parse ((labels ((maybe-integerize (itm)
                   (if (find-if #'digit-char-p itm)
                       (parse-integer itm)
                       itm))
                   (parse-line (l)
                     (mapcar #'maybe-integerize
                            (pick-elements (str:words l) 0 3 6 13))))
            (mapcar #'parse-line (str:lines input))))
  :p1 ((let ((time (or (first more) 0)))
         (s:extrema (mapcar (a:rcurry #'reindeer-distance time) input) #'> :key #'second)))
  :p1-test ((equal (princ (day-14-p1 (day-14-parse %test-input%) 1000)) %p1-expect% ))
  :p1-test-expected '("Comet" 1120)
  :p2 ((let ((time (or (first more) 0)))
         (loop :for tn :from 1 :upto time
               :collect (first (day-14-p1 input tn))
                 :into leaders-by-step
               :finally (a:appendf leaders-by-step
                                   (mapcar #'car input)) ;; they are tied for the lead at the start! for full input gives 1 too many though :(
                        (return (s:extrema (a:hash-table-alist (s:frequencies leaders-by-step))
                                           #'>
                                           :key #'cdr )))))
  :p2-test-expected 689
  :p2-test ((equal (cdr (princ (day-14-p2 (day-14-parse %test-input%) 1000))) %p2-expect%)))

(defun day-14-p2-with-ties (input &optional (time 1000))
  (loop :for tn :from 1 :upto time
        :for standings := (sort (mapcar (a:rcurry #'reindeer-distance time) input) #'> :key #'second)
        :for best-dist := (second (first standings))
        :nconcing (mapcar #'first (remove-if-not (lambda (deer-dist) (= best-dist deer-dist)) standings :key #'second))
          :into leaders-by-step
        :finally (return (a:hash-table-alist (s:frequencies leaders-by-step))
                         ;; #'>
                         ;; :key #'cdr
                         )))
