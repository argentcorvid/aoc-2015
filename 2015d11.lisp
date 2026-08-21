;;;2015 day 11

(in-package :aoc-2015)

(defparameter day-11-input "hepxcrrq")

(defun iol-p (char-in)
  (case char-in ((#\i #\o #\l) t)))

(defun no-iol-p (string-in)
  (notany #'iol-p string-in))

(defun straight-of-three-p (string-in)
  (search '(1 1) (s:deltas string-in (lambda (a b) (- (char-code a) (char-code b)))))
  ;; (loop :with alpha := "abcdefghijklmnopqrstuvwxyz"
  ;;       :for start :from 0
  ;;       :and end :from 3 :to (length alpha)
  ;;              :thereis (search alpha string-in :start1 start :end1 end)
  ;;       )
  )

(defun two-pairs-p (string-in)
  (<= 2
      (count-if
       (lambda (run)
         (>= (length run) 2))
       (s:runs string-in))))

(defun valid-password-p (string-in)
  (and (no-iol-p string-in)
       (straight-of-three-p string-in)
       (two-pairs-p string-in)))

(defun char-to-num (char-in)
  (- (char-code char-in) 97))

(defun num-to-char (int-in)
  (code-char (+ int-in 97)))

(defun next-char (char &optional (carry 0))
  (multiple-value-bind
        (carry out)
      (floor (+ (char-to-num char) 1 carry) 26)
    (values (num-to-char out) carry)))

(defun increment-password (string-in)
  (let* ((string-in (copy-seq string-in))
         (new-pwd
           (loop :with carry-in := 0
                 :for c :across (reverse string-in)
                 :for (new carry-out) := (multiple-value-list (next-char c))
                 :collect new :into out
                 :do (setf carry-in carry-out)
                 :until (zerop carry-in)
                 :finally (return (replace string-in (reverse out) :start1 (- (length string-in) (length out)))))))
    (a:if-let ((p (position-if #'iol-p new-pwd)))
      (increment-password (fill new-pwd #\z :start (1+ p)))
      new-pwd)))

(defday 11
  :test-input ""
  :parse ()
  :p1-test (())
  :p1 ((loop :for pw := (increment-password input) :then (increment-password pw)
             :for steps :from 1
             :until (valid-password-p pw)
             :finally (fresh-line)
                      (return (princ pw))))
  :p2 ((day-11-p1 (day-11-p1 input))))
