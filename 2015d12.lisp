;;;2015 day 12

(in-package :aoc-2015)

(defday 12
  :test-input ""
  :parse ()
  :p1 ((reduce #'+
               (mapcar #'parse-integer
                       (ppcre:all-matches-as-strings
                        "(?m)-?\\d+"
                        (uiop:read-file-string *day12input*)))))
  :p2 ((let ((data (jzon:parse (pathname *day12input*))))
         (labels ((sum-objs (obj)
                    (typecase obj
                      (hash-table (let ((vals (a:hash-table-values obj)))
                                    (if (find "red" vals)
                                        0
                                        (reduce #'+ (mapcar #'sum-objs vals)))))
                      (vector (reduce #'+ (map 'list #'sum-objs obj)))
                      (integer obj)
                      (t 0))))
           (sum-objs data)))))
