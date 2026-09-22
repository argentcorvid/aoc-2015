;;;2015 day 23

(in-package :aoc-2015)

(defparameter *register-names* #("a" "b" "c"))
(defvar *registers*)

(defun clear-registers ()
  (setf *registers*
        (s:pairhash *register-names*
                    (make-array 3 :element-type 'fixnum :initial-contents '(0 0 -1))
                    (s:dict 'equal))))

(s:-> register-value (string) fixnum)
(defun register-value (reg-name)        ; define a setf too!
  (gethash reg-name *registers*))

(defun (setf register-value) (val reg-name)
  (setf (gethash reg-name *registers*)
        val))

(defparameter *instruction-names* #("hlf" "tpl" "inc" "jmp" "jie" "jio"))

(s:-> (half triple) (fixnum) fixnum)

(defun half (number)
  (floor number 2))

(define-modify-macro halff () half)

(defun hlf (reg-name)
  (halff (register-value reg-name)))

(defun triple (number)
  (* 3 number))

(define-modify-macro triplef () triple)

(defun tpl (reg-name)
  (triplef (register-value reg-name)))

(s:-> inc (string) fixnum)
(defun inc (reg-name)
  (incf (register-value reg-name)))

(s:-> jmp (fixnum) fixnum)
(defun jmp (offset)
  (incf (register-value "c") (1- offset)))

(s:-> (jie jio) (string fixnum) (or null fixnum))
(defun jie (reg-name offset)
  (when (evenp (register-value reg-name))
    (incf (register-value "c") (1- offset))))

(defun jio (reg-name offset)
  (when (= 1 (register-value reg-name))
    (incf (register-value "c") (1- offset))))

(defparameter *instructions*
  (s:pairhash *instruction-names*
              (map 'vector (lambda (fn)
                        (symbol-function (find-symbol (string-upcase fn) :aoc-2015)))
                      *instruction-names*)
              (s:dict 'equal)))

(s:-> inst-func (string) function)
(defun inst-func (inst-name)
  (gethash inst-name *instructions*))

(s:-> d23-parse-line (string) list)
(defun d23-parse-line (line)
  (mapcar (lambda (str)
            (if (some #'digit-char-p str)
                (parse-integer str)
                str))
          (ppcre:split ",? " line)))

(defun fetch-execute (input)
  (catch 'halt
    (loop
      (incf (register-value "c"))
      (destructuring-bind (inst-name . args)
          (handler-bind ((type-error (lambda (c)
                                       (declare (ignore c))
                                       (throw 'halt nil))))
            (svref input (register-value "c")))
        (apply (inst-func inst-name) args)))))

(defday 23
  :test-input
  "inc a
jio a, +2
tpl a
inc a"
  :parse ((map 'vector #'d23-parse-line (str:lines input)))
  :p1 ((clear-registers)
       (fetch-execute input)
       (format t "~&~{~a: ~d~^, ~}" (a:hash-table-plist *registers*)))
  :p1-test ((day-23-p1 (day-23-parse %test-input%))
            (= (the fixnum %p1-expect%) (the fixnum (gethash "a" *registers*))))
  :p1-test-expected (the fixnum 2)
  :p2 ((clear-registers)
       (setf (register-value "a") 1)
       (fetch-execute input)
       (format t "~&~{~a: ~d~^, ~}" (a:hash-table-plist *registers*))))

(defun day-23-p1-run ()
  (day-23-p1 (day-23-parse (uiop:read-file-string *day23input*)))
  (fresh-line)
  (princ (gethash "b" *registers*)))

(defun day-23-p2-run ()
  (day-23-p2 (day-23-parse (uiop:read-file-string *day23input*)))
  (fresh-line)
  (princ (gethash "b" *registers*)))
