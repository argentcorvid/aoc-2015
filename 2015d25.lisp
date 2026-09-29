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


;;; free google ai explainer:
;; Think of it this way: triangular numbers represent the total area of a staircase grid.
;; Every time you add a new diagonal to this grid, you are gluing a new, longer layer onto the bottom of a staircase.
;; Let's look at the shape of the grid if we stop after completing each diagonal layer.
;; ------------------------------
;; ## Layer 1: Just Diagonal 1 (Size = 1)
;; We only fill (1,1). It forms a perfect mini-triangle of 1 block.

;; X  <-- Row 1, Col 1 (Total cells = 1)


;; * Total cells used: 1 (The 1st triangular number: $\frac{1 \times 2}{2} = 1$)

;; ------------------------------
;; ## Layer 2: Adding Diagonal 2 (Size = 2)
;; We add two new cells: (2,1) and (1,2). When you look at the whole shape, it forms a larger staircase triangle:

;; X O  
;; O    


;; * X is the old layer. O is the new diagonal layer.
;; * Total cells used: $1 + 2 =$ 3 (The 2nd triangular number: $\frac{2 \times 3}{2} = 3$)

;; ------------------------------
;; ## Layer 3: Adding Diagonal 3 (Size = 3)
;; We add three new cells: (3,1), (2,2), and (1,3). The staircase grows again:

;; X O #
;; O #
;; #


;; * # is the 3rd diagonal layer.
;; * Total cells used: $1 + 2 + 3 =$ 6 (The 3rd triangular number: $\frac{3 \times 4}{2} = 6$)

;; ------------------------------
;; ## The Mathematical Connection
;; A triangular number ($T_n$) is defined as the sum of all whole numbers from $1$ up to $n$:
;; $$T_n = 1 + 2 + 3 + \dots + n$$ 
;; Because each new diagonal line in your grid is exactly one cell longer than the diagonal line before it:

;; * Diagonal 1 has 1 cell.
;; * Diagonal 2 has 2 cells.
;; * Diagonal 3 has 3 cells.
;; * Diagonal 4 has 4 cells.

;; Therefore, if you want to know how many total numbers have been written down after completing the 4th diagonal, you must add up the sizes of the first four diagonals: $1 + 2 + 3 + 4 = 10$.
;; Because this geometric packing exactly mimics building a triangle out of blocks, the total count of cells at the end of any diagonal is always a perfect triangular number.
;; Would you like to try calculating a specific row and column together using this block-counting mindset to see it in action?

