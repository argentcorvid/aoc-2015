;;;2015 day 19

(in-package :aoc-2015)

(defday 19
  :test-input
  "H => HO
H => OH
O => HH

HOH
"
  :parse ((destructuring-bind (reps molecule)
              (str:paragraphs input)
            (let ((r-map (s:dict)))
              (dolist (line (str:lines reps))
                (apply (a:rcurry #'s:addhash r-map)
                       (str:split " => " line :regex t)))
              )))
  :p1 ()
  :p2 ())
