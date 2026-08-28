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
  (loop :with new-grid := (make-array (array-dimensions grid))
        :for i :below (array-total-size grid)
        :do (setf (row-major-aref new-grid i)
                  (apply #'grid-point-next-state grid (s:array-index-row-major grid i)))
        :finally (return new-grid)))

(defun grid-count-ons (grid)
  (count *d18-on* (make-array (array-total-size grid) :displaced-to grid)))

(defun grid-set-corners (grid)
  (destructuring-bind (max-rows max-cols)
          (array-dimensions grid)
        (setf (aref grid 0 0) *d18-on*
              (aref grid 0 (1- max-cols)) *d18-on*
              (aref grid (1- max-rows) 0) *d18-on*
              (aref grid (1- max-rows) (1- max-cols)) *d18-on*))
  grid)

(defun grid-after-steps (grid steps &optional (part-2 nil))
  (do ((grid (a:copy-array grid) (grid-next-state grid))
       (step 0 (1+ step)))
      ((= steps step)
       (if part-2 (grid-set-corners grid) grid))
    (when part-2
      (setf grid (grid-set-corners grid)))))

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
  :p2 ((grid-count-ons (grid-after-steps input (first more) t)))
  :p2-test-expected 17
  :p2-test ((let ((out (day-18-p2 (day-18-parse %test-input%) 5)))
              (values out 
                      (= %p2-expect% out)))))
