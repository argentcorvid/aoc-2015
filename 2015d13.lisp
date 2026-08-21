;;;2015 day 13

(in-package :aoc-2015)

(defun get-happiness (table seating-order)
  (let ((a (copy-list seating-order))
        (b (a:rotate (copy-list seating-order) -1)))
    (list seating-order
          (reduce #'+ (mapcan (lambda (person-a person-b)
                                (list (s:href table person-a person-b)
                                      (s:href table person-b person-a)))
                              a b)))))

(defday 13
  :test-input
  "Alice would gain 54 happiness units by sitting next to Bob.
Alice would lose 79 happiness units by sitting next to Carol.
Alice would lose 2 happiness units by sitting next to David.
Bob would gain 83 happiness units by sitting next to Alice.
Bob would lose 7 happiness units by sitting next to Carol.
Bob would lose 63 happiness units by sitting next to David.
Carol would lose 62 happiness units by sitting next to Alice.
Carol would gain 60 happiness units by sitting next to Bob.
Carol would gain 55 happiness units by sitting next to David.
David would gain 46 happiness units by sitting next to Alice.
David would lose 7 happiness units by sitting next to Bob.
David would gain 41 happiness units by sitting next to Carol."
  :p1-test-expected 330
  :parse ((loop :with ht := (s:dict)
                :for line string :in input
                :for splits := (s:words line :end (1- (length line)))
                :do (setf (gethash (a:lastcar splits) (a:ensure-gethash (first splits) ht (s:dict)))
                          (parse-integer (s:concat (if (string= (third splits) "gain")
                                                       "+"
                                                       "-")
                                                   (fourth splits))))
                :finally (return ht)))
  :p1 ((let (totals)
         (a:map-derangements
          (lambda (seating-order)
            (push (get-happiness input seating-order) totals))
          (a:hash-table-keys input))
         (s:extrema totals #'> :key #'second)))
  :p1-test ((destructuring-bind (order happiness)
                (day-13-p1 (day-13-parse (s:lines %test-input%)))
              (format t "~&Max happiness: ~a" happiness)
              (format t "~&Order: ~a" order)
              (= happiness %p1-expect%)))
  :p2 ((loop :for k :being :the :hash-keys :of input
             :do (setf (s:href input k "Me") 0
                       (gethash k (a:ensure-gethash "Me" input (s:dict))) 0))
       (day-13-p1 input)))


