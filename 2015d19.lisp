;;;2015 day 19
;;;https://www.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/cy4cq4j/


(in-package :aoc-2015)

(S:-> next-molecule (string hash-table) hash-table)
(defun next-molecule (molecule r-map)
  (loop :with new-molecules := (s:dict)
        :for ch :across molecule
        :and i :from 0
        :do (loop :for r :in (or (gethash (string ch) r-map)
                                 (gethash (s:slice molecule i (+ 2 i)) r-map))
                  :when r
                    :do (setf (gethash (str:concat (s:slice molecule 0 i)
                                                   r
                                                   (s:slice molecule (1+ i )))
                                       new-molecules)
                              t))            
        :finally (return new-molecules)))

(defun count-molecules (molecule r-map)
  (hash-table-count (next-molecule molecule r-map)))

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
              (values molecule r-map))))
  :p1 ((multiple-value-call #'count-molecules (day-19-parse input)))
  :p1-test ((let ((r (day-19-p1 %test-input%)))
              (values r (= r %p1-expect%))))
  :p1-test-expected 4
  :p2 ())
