;;;2015 day 9

(in-package :aoc-2015)

(defun pick-elements (in-seq &rest indices)
  (let ((out (list)))
    (dotimes (i (length indices))
      (push (elt in-seq (elt indices i)) out))
    (nreverse out)))

(defun day-9-distances (city-list pair-lookup)
  (let ((all-route-distances (list)))
    (a:map-permutations
     (lambda (route-order)
       (let ((a (butlast route-order))
             (b (rest route-order)))
         (push
          (reduce #'+ (mapcar (lambda (city-a city-b)
                                (gethash (list city-a city-b) pair-lookup))
                              a b))
          all-route-distances)))
     city-list)
    all-route-distances))

(defday 9
  :test-input '("London to Dublin = 464"
                "London to Belfast = 518"
                "Dublin to Belfast = 141")
  
  :parse ((loop :with all-cities := (list)
                :and distances := (make-hash-table :test 'equal)
                :for line :in input
                :for parts := (str:words line)
                :for route-cities := (pick-elements parts 0 2)
                :do (a:unionf all-cities route-cities :test #'string=)
                    (let ((dist (parse-integer (nth 4 parts))))
                      (setf (gethash route-cities distances)
                            dist
                            (gethash (reverse route-cities) distances)
                            dist))
                :finally (return (values all-cities distances))))
  :p1 ((princ (reduce #'min (day-9-distances input (first more)))))
  :p1-test ((multiple-value-call #'day-9-p1 (day-9-parse %test-input%)))
  :p1-test-expected 605
  :p2 ((princ (reduce #'max (day-9-distances input (first more)))))
  :p2-test ((multiple-value-call #'day-9-p2 (day-9-parse %test-input%)))
  :p2-test-expected 982)
