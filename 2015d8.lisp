;;;2015 day 8

(in-package :aoc-2015)

(defparameter *d8-escaped-hex* (ppcre:create-scanner "(?<!\\\\){1}\\\\x([0-9a-f]{2})"))

(defun unescape-hex-match (match number)
  (declare (ignore match))
  (format nil "~a" (code-char (parse-integer number :radix 16))))

(defun count-chars (str)
  (if (find #\\ str)
      (let ((str (copy-seq str)))
       (ppcre:regex-replace-all *d8-escaped-hex*
                                str
                                #'unescape-hex-match
                                :simple-calls t)
       (ppcre:regex-replace-all (ppcre:quote-meta-chars "\\\\") 
                               str
                               (ppcre:quote-meta-chars "\\"))
       (ppcre:regex-replace-all (ppcre:quote-meta-chars "\\\"")
                                str
                                (ppcre:quote-meta-chars "\""))
      )
      (cons (length str) (- (length str) 2))))



(defday 8
  :test-input '("\"\""
                "\"abc\""
                "\"aaa\\\"aaa\""
                "\"\\x27\"")
  :parse ()
  :p1 ((let ((single-escape (ppcre:create-scanner "\\\\\\\\|\\\\\\\""))
             (hex-escape (ppcre:create-scanner "(?<!\\\\)\\\\x[0-9a-z]{2}"))
             (code-length 0)
             (string-length 0))
         (dolist (line input) ;;; change to reduce
           (let ((singles-count  (ppcre:count-matches single-escape line))
                 (hex-count (ppcre:count-matches hex-escape line)))
             (format t "~&~a :~%code length: ~d~%singles found: ~d~%hexes found: ~d~%string length: ~d~%~%"
                     line
                     (length line)
                     singles-count
                     hex-count
                     (- (length line) 2 singles-count (* 3 hex-count)))
             (incf code-length (length line))
             (incf string-length (- (length line) 2 singles-count (* 3 hex-count)))))
         (- code-length string-length)))
  :p2 ())
