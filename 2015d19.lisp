;;;2015 day 19
;;;https://www.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/cy4cq4j/


(in-package :aoc-2015)

(defun next-molecule (molecule r-map)
  (loop :for ch :across molecule
        :and i :from 0
        :for r := (gethash ch r-map)))

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
              (values r-map molecule))))
  :p1 ()
  :p2 ())
