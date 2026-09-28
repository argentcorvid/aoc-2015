;;;2015 day 25

(in-package :aoc-2015)

(defun number-to-generate (row col)
  (let* ((side-length (+ row col -1))
         (area (floor (expt side-length 2) 2))
         (first-on-hyp (1- area)))
    (+ col first-on-hyp -1)))

(defun iterate-codes (starting-code times)
  (iter
    (for code first  starting-code then (mod (* 252533 code) 33554393))
    (repeat times)
    (finally (return code))))

(let ((exptmod-cache (make-hash-table :test 'equalp)))
  (defun exptmod (base exp mod)
    (a:if-let (cached-result (gethash (vector base exp mod) exptmod-cache))
      cached-result
      (setf (gethash (vector base exp mod) exptmod-cache)
            (multiple-value-bind (q r)
                (floor exp 2)
              (cond ((zerop exp)
                     (return-from exptmod 1))
                    ((zerop r)
                     (mod (expt (exptmod base q mod) 2) mod))
                    (t
                     (mod (* base (exptmod base (1- exp) mod)) mod))))))))

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
