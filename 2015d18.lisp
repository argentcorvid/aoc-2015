;;;2015 day 18

(in-package :aoc-2015)

(defparameter *d18-on* #\#)
(defparameter *d18-off* #\.)

(defun grid-neigbors-8 (row col)
  (loop :for look-row :from (1- row) :upto (1+ row)
        :nconc (loop :for look-col :from (1- col) :upto (1+ col)
                     :unless (and (= look-row row)
                                  (= look-col col))
                       :collect (list look-row look-col))))

(defun grid-point-state (grid row col)
  (handler-case (aref grid row col)
    (type-error nil *d18-off*)))

(defun grid-count-on-neighbors (grid row col)
  (count *d18-on* (s:mapply (a:curry #'grid-point-state grid)
                          (grid-neigbors-8 row col))))

(defun grid-point-next-state (grid row col)
  (let ((ons (grid-count-on-neighbors grid row col)))
    (if (eql *d18-on* (grid-point-state grid row col))
        (if (or (= 2 ons)
                (= 3 ons ))
            *d18-on*
            *d18-off*)
        (if (= 3 ons)
            *d18-on*
            *d18-off*))))

(defun grid-next-state (grid)
  (loop :with (max-rows max-cols) (fixnum fixnum) := (array-dimensions grid)
        :for row :from 0 :below max-rows
        :collect (loop :for col :from 0 :below max-cols
                     :collect (grid-point-next-state grid row col))
          :into new-state
        :finally (return (make-array (list max-rows max-cols)
                                     :initial-contents new-state))))

(defun grid-count-ons (grid)
  (count *d18-on* (make-array (array-total-size grid) :displaced-to grid)))

(defun grid-after-steps (grid steps)
  (do ((grid grid (grid-next-state grid))
       (step 0 (1+ step)))
      ((<= steps step)
       grid)))

(defday 18
  :test-input
  ".#.#.#
...##.
#....#
..#...
#.#..#
####..
"
  :parse ((make-array (list (position #\newline input)
                            (count #\newline input))
                      :initial-contents (s:lines input)))
  :p1-test-expected 4
  :p1-test ((let ((out (day-18-p1 (day-18-parse %test-input%) 4)))
              (values out 
                      (= %p1-expect% out))))
  :p1 ((grid-count-ons (grid-after-steps input (first more))))
  :p2 ())
