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
  :p2 ())
