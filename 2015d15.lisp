;;;2015 day 15

(in-package :aoc-2015)

(defun score-cookie (ingredient-amounts ingredient-stats &optional (part-1 t))
  (loop :with stats := (if part-1
                           (mapcar (a:rcurry #'s:slice 0 -1) ingredient-stats)
                           ingredient-stats)
        :for (ing amt) :in ingredient-amounts
        :collect (mapcar (lambda (stat)
                           (* amt stat))
                         (s:assocdr ing
                                    stats
                                    :test #'string=))
          :into score-components
        :finally (return (reduce #'*
                                 (apply #'mapcar #'+ score-components)
                                 :key (lambda (s) (if (minusp s) 0 s))))))

(defday 15
  :test-input
  (str:lines
   "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3")
  :parse ((let (table)
            (dolist (line input table)
              (destructuring-bind (name &rest stats)
                  (str:words line)
                (setf table (acons (s:slice name 0 -1)
                                   (mapcar (a:rcurry #'parse-integer :junk-allowed t)
                                           (pick-elements stats 1 3 5 7 9))
                                   table))))))
  :p1 ((let ((ingredient-budget 100)
             (ingredient-names (mapcar #'first input)))
         ))
  :p2 ())
