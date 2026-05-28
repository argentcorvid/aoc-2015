;;;2015 day 8

(in-package :aoc-2015)

(defday 8
  :test-input '("\"\""
                "\"abc\""
                "\"aaa\\\"aaa\""
                "\"\\x27\"")
  :parse ()
  :p1 ((let ((single-escape (ppcre:create-scanner "\\\\\\\\|\\\\\\\""))
             (hex-escape (ppcre:create-scanner "(?<!(?<!\\\\)\\\\)\\\\x[0-9a-f]{2}"))
             (code-length 0)
             (string-length 0))
         (dolist (line input) ;;; change to reduce
           (let ((singles-count  (ppcre:count-matches single-escape line))
                 (hex-count (ppcre:count-matches hex-escape line)))
             (vformat t "~&~a :~%code length: ~d~%singles found: ~d~%hexes found: ~d~%string length: ~d~%~%"
                     line
                     (length line)
                     singles-count
                     hex-count
                     (- (length line) 2 singles-count (* 3 hex-count)))
             (incf code-length (length line))
             (incf string-length (- (length line) 2 singles-count (* 3 hex-count)))))
         (- code-length string-length)))
  :p2 ((let ((code-length 0)
             (new-length 0))
         (dolist (line input)
           (incf code-length (length line))
           (incf new-length (+ (length line) 2 (ppcre:count-matches "\\\\|\\\"" line))))
         (- new-length code-length))))
