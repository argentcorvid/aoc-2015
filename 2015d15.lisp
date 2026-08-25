;;;2015 day 15

(in-package :aoc-2015)

(defun score-cookie (ingredient-amounts ingredient-stats)
  (loop :for (ing amt) :in ingredient-amounts
        :collect (mapcar (lambda (stat)
                           (* amt stat))
                         (s:assocdr ing
                                    ingredient-stats
                                    :test #'string=))
          :into score-components
        :finally (return (reduce #'*
                                 (apply #'mapcar #'+ score-components)
                                 :key (lambda (s) (if (not (minusp s)) s 0))))))

(s:-> partition (fixnum fixnum) list)
(let ((cache (make-hash-table :test 'equal)))
  (defun partition (total ways)
    (multiple-value-bind (val present)
        (gethash (list total ways) cache)
      (if present
          val
          (setf (gethash (list total ways) cache)
                (cond ((zerop ways) nil)
                      ((= 1 ways)
                       (list (list total)))
                      (t (loop :for h fixnum :in (a:iota (1+ total))
                               :nconc (loop :for tt :in (partition (- total h) (1- ways))
                                            :collect (append (list h) tt))))))))))

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
  :p1 ((let* ((ingredient-budget 100)
              (ingredient-names (mapcar #'first input))
              (stats  (mapcar (a:rcurry #'s:slice 0 -1) input)) ;; "ignore calories *for now*"
              )
         (loop :for amounts :in (partition ingredient-budget (length ingredient-names))
               :maximize (score-cookie (mapcar #'list ingredient-names amounts) stats))))
  :p1-test-expected 62842880
  :p1-test ((let ((v (day-15-p1 (day-15-parse %test-input%))))
              (values v (= %p1-expect% v))) )
  :p2 ((loop :with ingredient-budget := 100
             :and ingredient-names := (mapcar #'first input)
             :and stats := (mapcar #'butlast input)
             :and calories := (mapcar #'a:last-elt input)
             :for amounts :in (partition ingredient-budget (length ingredient-names))
             :when (= 500 (reduce #'+ (mapcar #'* calories amounts)))
               :maximize (score-cookie (mapcar #'list ingredient-names amounts) stats)))
  :p2-test-expected 57600000
  :p2-test ((let ((v (day-15-p2 (day-15-parse %test-input%))))
              (values v (= %p2-expect% v)))))
