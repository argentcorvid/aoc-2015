;;;2015 day 25

(in-package :aoc-2015)

(defun number-to-generate (row col)
;;; area of previous triangle + col
;;; or area of current - row
  (let* ((diagonal-number (+ row col -1)) ;; every co-ordinate on the same diagonal as given co-ordinates adds up to this
         (area-of-prev (/ (* (1- diagonal-number) diagonal-number) 2)) ;; triangular number, because it's discrete blocks.
         )
    (+ area-of-prev col -1) ;; move along new diagonal to needed column, (and adjust for 1 base instead of 0?)
    ))

(defun iterate-codes (starting-code times)
  (iter
    (for code first  starting-code then (mod (* 252533 code) 33554393))
    (repeat times)
    (finally (return code))))

(defun exptmod (base exp mod)
  (when (minusp exp)
    (error "negative exponent!"))
  (multiple-value-bind (q r)
      (floor exp 2)
    (cond ((zerop exp)
           (return-from exptmod 1))
          ((zerop r)
           (mod (expt (exptmod base q mod) 2) mod))
          (t
           (mod (* base (exptmod base (1- exp) mod)) mod)))))

(defun exptmod-codes (starting-code times)
  (let ((base 252533)
        (exp times)
        (mod 33554393))
    (mod (* (exptmod base exp mod) starting-code) mod)))

(defday 25
  :parse ((let* ((last-dot-pos (position #\. input :from-end t))
                 (col-start-pos (position #\space input :from-end t :end last-dot-pos))
                 (last-comma-pos (position #\, input :from-end t))
                 (row-start-pos (position #\space input :from-end t :end last-comma-pos)))
            (values (parse-integer input :start row-start-pos :end last-comma-pos)
                    (parse-integer input :start col-start-pos :end last-dot-pos))))
  :p1 ((exptmod-codes 20151125
                      (multiple-value-call #'number-to-generate
                        (day-25-parse (uiop:read-file-string *day25input*)))))
  :p2 ())

(defun time-d25-p1 ()
  (let ((number (multiple-value-call #'number-to-generate
              (day-25-parse (uiop:read-file-string *day25input*))))
        (start-code 20151125))
    (fresh-line)
    (princ "iterate:")
    (fresh-line)
    (princ (time (iterate-codes start-code number)))
    (fresh-line)
    (princ "exptmod:")
    (fresh-line)
    (princ (time (exptmod-codes start-code number)))))
