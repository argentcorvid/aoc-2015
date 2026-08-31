;;;2015 day 19
;;;https://www.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/cy4cq4j/


(in-package :aoc-2015)

(S:-> next-molecule (string hash-table) hash-table)
(defun next-molecule (molecule r-map)
  (loop :with new-molecules := (s:dict)
        :for ch :across molecule
        :and i :from 0
        :do (a:if-let (reps (gethash (string ch) r-map))
              (dolist (r reps)
                (setf (gethash (str:concat (s:slice molecule 0 i)
                                           r
                                           (s:slice molecule (+ i 1)))
                               new-molecules)
                      t))
              (a:when-let (reps (gethash (s:slice molecule i (+ i 2)) r-map))
                (dolist (r reps)
                  (setf (gethash (str:concat (s:slice molecule 0 i)
                                             r
                                             (s:slice molecule (+ i 2)))
                                 new-molecules)
                        t))))            
        :finally (return new-molecules)))

(defun count-molecules (molecule r-map)
  (hash-table-count (next-molecule molecule r-map)))

(defun prev-molecule (molecule r-map)
  (let ((new-molecules (s:dict)))
    (maphash
     (lambda (k v)
       (loop :for i := (search k molecule) :then (search k molecule :start2 (1+ i))
             :while (and (not (null i))
                         (>= i 0))
             :do (dolist (r v)
                   (unless (string= r "e")
                     (handler-case
                         (setf (gethash (str:concat (s:slice molecule 0 i)
                                                    r
                                                    (s:slice molecule (+ i (length k))))
                                        new-molecules)
                               t)
                       (type-error nil (setf (gethash (str:concat (s:slice molecule 0 i)
                                                                  r)
                                                      new-molecules)
                                             t)))))))
     r-map)
    (when (zerop (hash-table-count new-molecules))
      (setf new-molecules (s:dict "e" t)))
    new-molecules))

(defun invert-map (r-map-in)
  (let ((revmap (s:dict)))
    (s:do-hash-table (k v r-map-in revmap)
      (dolist (i v)
        (s:addhash i k revmap)))))

(defun steps-to-generate (molecule r-map)
  (loop :with r-map := (invert-map r-map)
        :and seen := (s:dict)
        :while (not (equalp prev-gen (s:dictq "e" t)))
        :for curr-gen := (s:dict) :then (s:merge-tables curr-gen new)
        :for steps :from 1
        :for  prev-gen := (prev-molecule molecule r-map) :then  curr-gen
        :for candidate := (s:shortest (a:hash-table-keys prev-gen))
        :for new := (s:href seen candidate)
        :unless new
          :do (setf new (prev-molecule candidate r-map))
              (setf (s:href seen candidate) new)
                                        ;(s:merge-tables* curr-gen candidate)
        :finally (return steps)))

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
                       (str:split " => " line)))
              (values molecule r-map))))
  
  :p1 ((multiple-value-call #'count-molecules (day-19-parse input)))
  :p1-test ((let ((r (day-19-p1 %test-input%)))
              (values r (= r %p1-expect%))))
  :p1-test-expected 4
  
  :p2 ((multiple-value-call #'steps-to-generate (day-19-parse input)))
  :p2-test-expected '(3 6)              ; HOH, HOHOHO
  :p2-test ((let (outs)
              (dolist (in (list "e => H
e => O
H => HO
H => OH
O => HH

HOH
"
                                "e => H
e => O
H => HO
H => OH
O => HH

HOHOHO
"))
                (push (multiple-value-call #'steps-to-generate (day-19-parse in))
                      outs))
              (values outs (a:set-equal outs %p2-expect% :test #'equal))))
  )
