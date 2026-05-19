;;;2015 day 4

(in-package :aoc-2015)

(require 'sb-md5) ;;no, i'm not implementing this myself

(defparameter *day-4-input* "yzbqklnj")

(defun md5vec->str (md5vec)
  (format nil "~{~2,'0x~}" (coerce (subseq md5vec 0 4) 'list)))

(defday 4
  :test-input '("abcdef"
                "pqrstuv")
  :p1-test-expected '(609043
                      1048970)
  :parse ()
  :p1 ((loop :for ans :from 1
             :for md5 := (sb-md5:md5sum-string (format nil "~a~a" input ans))
             :when (and (zerop (aref md5 0))
                        (zerop (aref md5 1))
                        (< (aref md5 2) #x10))
               :return ans))
  :p2 ((loop :for ans :from 1
             :when (equalp #(0 0 0)
                          (subseq (sb-md5:md5sum-string (format nil "~a~a" input ans))
                                  0 3))
               :return ans)))
