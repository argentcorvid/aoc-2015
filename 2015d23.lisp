;;;2015 day 23

(in-package :aoc-2015)

(defparameter *register-names* #("a" "b" "c"))
(defvar *registers*)

(defun set-registers (&key (a 0) (b 0) (c -1))
  (setf *registers*
        (s:pairhash *register-names*
                    (vector a b c)
                    (s:dict 'equal))))

(defun clear-registers ()
  (set-registers))

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

(s:-> (jie jio) (string fixnum) fixnum)
(defun jie (reg-name offset)
  (if (evenp (register-value reg-name))
      (incf (register-value "c") (1- offset))
      0))

(defun jio (reg-name offset)
  (if (= 1 (register-value reg-name))
      (incf (register-value "c") (1- offset))
      0))

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
  (loop
    (incf (register-value "c"))
    (apply (lambda (inst-name &rest args)
             (apply (inst-func inst-name) args))
           (handler-bind ((type-error (lambda (c)
                                        (declare (ignore c))
                                        (return-from nil))))
             (svref input (register-value "c"))))))

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
  :p2 ((set-registers :a 1)
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
