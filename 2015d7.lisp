;;;2015 day 7

(in-package :aoc-2015)

(defun make-number-or-symbol (str)
  (if (some #'digit-char-p str)
      (16bits (parse-integer str))
      (intern str)))

(defun 16bits (number)
  (ldb (byte 16 0) number))

(defparameter d7cache (make-hash-table))

(defun recurse-tree (from)
  (declare (special input))
  (multiple-value-bind (val present)
      (gethash from d7cache)
    (if present
        (16bits val)
        (let ((current (gethash from input)))
          (setf (gethash from d7cache)
                (16bits
                 (match current
                   ((and (type integer) num)
                    num)
                   ((and (type symbol) sym)
                    (recurse-tree sym))
                   ((list 'lognot sym)
                    (lognot (recurse-tree sym)))
                   ((list op arg1 arg2)
                    (let ((a1 (etypecase arg1
                                (symbol (recurse-tree arg1))
                                (integer arg1)))
                          (a2 (etypecase arg2
                                (symbol (recurse-tree arg2))
                                (integer arg2))))
                      (funcall op a1 a2)))
                   (_
                    (format t "no match for: ~a" current)))))))))

(defday 7
  :test-input ""
  :parse ((let ((nodes (make-hash-table :size (length input))))
            (dolist (line input nodes)
              (let ((tokens (reverse (str:words line))))
                (setf (gethash (intern (first tokens)) nodes)
                      (match (cddr tokens)
                        ((list in1 "NOT")
                         `(lognot ,(intern in1)))
                        ((list in2 (ppcre "(\\w)SHIFT" op) in1)
                         `(ash ;; left if positive
                           ,(intern in1)
                           ,(* -1 (if (string= op "R")
                                      1
                                      -1)
                               (parse-integer in2))))
                        ((list in2 op in1)
                         (list (str:string-case op
                                 ("AND" 'logand)
                                 ("OR" 'logior))
                               (make-number-or-symbol in1)
                               (make-number-or-symbol in2)))
                        ((list in1)
                         (make-number-or-symbol in1))))))))
  :p1 ((declare (special input))
       (clrhash d7cache)
       (recurse-tree '|a|))
  :P2 ((declare (special input))
       (clrhash d7cache)
       (let ((a (recurse-tree '|a|)))
         (setf (gethash '|b| input) a))
       (clrhash d7cache)
       (recurse-tree '|a|)))
