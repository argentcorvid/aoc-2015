;;;2015 day 10

(in-package :aoc-2015)

(defparameter day-10-input "1321131112")

(defun look (string-in)
  (mapcar
   (lambda (run)
     (list (length run)
           (remove-duplicates run :test #'char=)))
   (s:runs string-in :test #'char=) ))

(defun say (list-in)
  (map 'string (lambda (char-or-num)
                 (typecase char-or-num
                   (integer (code-char (+ char-or-num 48)))
                   (string (char char-or-num 0))))
       list-in))

(defun day-10-process (sequence times)
  (do* ((times times (1- times))
        (sequence sequence (say (a:flatten working)))
        (working (look sequence) (look sequence)))
       ((zerop times)
        (vformat t "~&Final Sequence: ~a" sequence)
        (format t "~&length ~a" (length sequence))
        (length sequence))
    (vformat t "~&Sequence: ~a" sequence)))

(defday 10
  :test-input "1"
  :p1-test-expected 6
  :p1-test ((= %p1-expect% (day-10-process %test-input% 4)))
  :p1 ((day-10-process input 40))
  :p2 ((day-10-process input 50)))
