;;;2015 day 6

(in-package :aoc-2015)

(defday 6
  :test-input ""
  :parse (())
  :p1 ((let* ((lights (make-array '(1000 1000) :element-type 'bit))
              (light-vec (make-array (* 1000 1000) :displaced-to lights)))
         (dolist (inst input) (count 1 light-vec)
           )))
  :p2 ())
