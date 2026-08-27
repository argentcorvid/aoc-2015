;;;2015 day 17

(in-package :aoc-2015)

(defday 17
  :p1 ((count-if (lambda (containers)
                   (= 150 (apply #'+ (mapcar #'parse-integer containers))))
                 (rest (s:powerset (uiop:read-file-lines *day17input*)))))
  :p2 ((let* ((valid-containers
               (remove-if-not
                (lambda (containers)
                  (= 150 (apply #'+ (mapcar #'parse-integer containers))))
                (rest (s:powerset (uiop:read-file-lines *day17input*)))))
              (minimum-containers (apply #'min (mapcar #'length valid-containers))))
         (count-if (lambda (itm) (= (length itm) minimum-containers)) valid-containers))))
