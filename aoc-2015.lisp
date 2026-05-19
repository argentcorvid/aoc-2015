;;;; aoc-2015.lisp
;;
;;;; Copyright (c) 2026 Kyle Hokanson <khokanson@gmail.com>


(in-package #:aoc-2015)

(defparameter *verbose* nil)
(defparameter *year* "2015")

(defmacro defday (day-number
                  &key ((:p1 (&body p1-body)))
                    ((:p2 (&body p2-body)))
                    ((:parse (&body parse-body)))
                    ((:p1-test (&body p1test-body)))
                    ((:p2-test (&body p2test-body)))
                    test-input
                    p1-test-expected
                    p2-test-expected)
  "Define a set of functions and test data in package \"AOC-<*year*>\" for a given day of Advent of Code.
day-number is a number
any of the keyword parameters :p1 :p2 and :parse will define functions with the name \"DAY-<day-number>-<keyword>\"
:test-input defines a local variable %test-input% for testing the examples, should be a list of separate test inputs
:p<num>-test-expected defines local variables %p1-expect% or %p2-expect% for testing examples, also should be lists
a global variable with name \"*DAY<day-number>INPUT*\" is defined containing the string \"<*year*>d<day-number>input.txt\""
  (let ((package (find-package (string-upcase (format nil "aoc-~d" *year*))))
        (names-and-bodies (remove-if #'null (list (cons "DAY-~d-P1" p1-body)
                                                  (cons "DAY-~d-P2" p2-body)
                                                  (cons "DAY-~d-PARSE" parse-body)
                                                  (cons "DAY-~d-P1TEST" p1test-body)
                                                  (cons "DAY-~d-P2TEST" p2test-body))
                                     :key #'cdr)))
    `(let ((%p1-expect% ,p1-test-expected)
           (%p2-expect% ,p2-test-expected)
           (%test-input% ,test-input))
       (declare (ignorable %p1-expect% %p2-expect% %test-input%))
       (progn
         (defparameter ,(intern (format nil "*DAY~dINPUT*" day-number)) ,(format nil "~dd~dinput.txt" *year* day-number))
         ,@(mapcar (lambda (name/body)
                     `(defun ,(intern (format nil (car name/body) day-number) package) (&optional input)
                        (declare (ignorable input))
                        ,@(cdr name/body)))
                   names-and-bodies)))))

(defun vformat (format-string &rest args)
  "print to standard output only if *verbose* is true"
  (when *verbose*
    (apply #'format t format-string args)))

(defun run (parts-list data)
  (dolist (part (a:ensure-list parts-list))
    (ccase part
      (1 (format t "~&Part 1: ~a" (p1 data)))
      (2 (format t "~&Part 2: ~a" (p2 data))))))

(defun main (&rest parts)
  (let* ((infile-name (format nil *input-name-template* *day-number*))
         (input-lines (uiop:read-file-lines infile-name))
         (data (parse-input input-lines)))
    (run parts data)))

(defun test (&rest parts)
  (let ((*verbose* t))
    (run parts (parse-input *test-input*))))
