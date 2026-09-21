;;;2015 day 23

(in-package :aoc-2015)

(defparameter *register-lookup* #("a" "b" "c"))
(defparameter *registers* (make-array (length *register-lookup*)
                                      :element-type 'fixnum
                                      :initial-element 0))

(defun reg (reg-name) ; define a setf too!
  (aref *registers* (position reg-name *register-lookup :test #'string=)))

(defparameter *instruction-lookup* #("hlf" "tpl" "inc" "jmp" "jie" "jio"))

(defun hlf (reg-name)
  (floor  2))

(defun tpl (reg-name)
  )

(defparameter *instructions* (make-array (length *instruction-lookup*)))



(defday 23
  :test-input ""
  :parse ()
  :p1 ()
  :p2 ())
