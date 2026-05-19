;;;2015 day 5

(in-package :aoc-2015)

(defun three-vowels-p (str)
  (flet ((vowelp (char)
           (find char "aeiou" :test #'char=)))
   (<= 3 (count-if #'vowelp str))))

(defun has-repeat-p (str)
  (loop :for idx :from 0 :below (1- (length str))
        :thereis (char= (aref str idx) (aref str (1+ idx)))))

(defun no-bad-combination-p (str &optional (bads (list "ab" "cd" "pq" "xy")))
  (loop :for b :in bads
        :never (search b str :test #'string=)))

(defun has-pairs-p (str)
  (ppcre:scan "(..).*\\1" str))

(defun has-split-pair-p (str)
  (ppcre:scan "(.).\\1" str))

(defday 5
  :test-input '("ugknbfddgicrmopn"
                "aaa"
                "jchzalrnumimnmhp"
                "haegwjzuvuyypxyu"
                "dvszwmarrgswjxmb")
  :p1-test-expected 2 ;; '(t t nil nil nil)
  :p1-test ((= %p1-expect% (day-5-p1 %test-input%)))
  :p1 ((count-if (a:conjoin #'three-vowels-p
                            #'has-repeat-p
                            #'no-bad-combination-p)
                 input))
  :p2 ((count-if (a:conjoin #'has-pairs-p #'has-split-pair-p) input)))
