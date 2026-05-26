;;;; aoc-2015.asd
;;
;;;; Copyright (c) 2026 Kyle Hokanson <khokanson@gmail.com>


(asdf:defsystem #:aoc-2015
  :description "Describe aoc-2015 here"
  :author "Kyle Hokanson <khokanson@gmail.com>"
  :license  "BSD 2-Clause"
  :version "0.0.1"
  :serial t
  :depends-on ("alexandria" "ppcre" "str" "sb-md5" "trivia" "trivia.ppcre")
  :components ((:file "package")
               (:file "aoc-2015")
               (:file "2015d1")
               (:file "2015d2")
               (:file "2015d3")
               (:file "2015d4")
               (:file "2015d5")
               (:file "2015d6")))
