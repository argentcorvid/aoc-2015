;;;; aoc-2015.asd
;;
;;;; Copyright (c) 2026 Kyle Hokanson <khokanson@gmail.com>


(asdf:defsystem #:aoc-2015
  :description "Describe aoc-2015 here"
  :author "Kyle Hokanson <khokanson@gmail.com>"
  :license  "BSD 2-Clause"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria-2 #:str)
  :components ((:file "package")
               (:file "aoc-2015")))
