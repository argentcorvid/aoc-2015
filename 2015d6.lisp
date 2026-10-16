;;;2015 day 6

(in-package :aoc-2015)

(defday 6
  :test-input '("turn on 0,0 through 999,999"
                "toggle 0,0 through 999,0"
                "turn off 499,499 through 500,500")
  :parse ()
  :p1 ((let* ((lights (make-array '(1000 1000) :element-type 'bit :initial-element 0))
              (scanner (ppcre:create-scanner "(e|n|f) (\\d{1,}),(\\d{1,}) \\w+ (\\d{1,}),(\\d{1,})")))
         (dolist (inst input
                       (count 1 (make-array (array-total-size lights)
                                            :displaced-to lights :element-type 'bit)))
           (ppcre:register-groups-bind
               (((a:compose #'intern #'string-upcase) action)
                (#'parse-integer c1-x c1-y c2-x c2-y))
               (scanner inst)
             (loop :for x :from c1-x :to c2-x
                   :do (loop :for y :from c1-y :to c2-y
                             :do (setf (sbit lights x y)
                                       (case action
                                         (e (case (sbit lights x y)
                                              (0 1)
                                              (1 0)))
                                         (n 1)
                                         (f 0)))))))))
  :p2 ((let ((scanner (ppcre:create-scanner "(e|n|f) (\\d{1,}),(\\d{1,}) \\w+ (\\d{1,}),(\\d{1,})"))
             (lights (make-array '(1000 1000) :element-type 'fixnum)))
         (dolist (inst input (reduce #'+ (make-array (array-total-size lights) :displaced-to lights :element-type 'fixnum)))
           (ppcre:register-groups-bind
               (((a:compose #'intern #'string-upcase) action)
                (#'parse-integer c1-x c1-y c2-x c2-y))
               (scanner inst)
             (let ((delta (case action
                            (e 2)
                            (n 1)
                            (f -1))))
               (loop :for x :from c1-x :to c2-x
                     :do (loop :for y :from c1-y :to c2-y
                               :do (incf (aref lights x y)
                                         delta)
                                   (when (minusp (aref lights x y))
                                     (setf (aref lights x y) 0))))))))))

(defun day-6-p1-simd (input-lines)
  (let* ((lights (make-array '(1000 1000) :element-type 'simd:u8 :initial-element (simd:u8 0)))
         (dlights (make-array (array-total-size lights) :element-type 'simd:u8 :displaced-to lights)))
    (macrolet ((dispatch-action (simd-action scalar-action)
                 `(iter grid-rows
                    (for row from top-row to bot-row)
                    (iter grid-cols
                      (for col from left-col by 16)
                      (repeat times)
                      (setf (simd:u8.16-aref lights row col)
                            ,simd-action)
                      (finally (iter tail
                                 (for tail-col from col)
                                 (repeat (1+ rem))
                                 (setf (aref lights row tail-col)
                                       ,scalar-action)))))))
      (iter instructions
        (for raw-line in input-lines)
        (for (action left-col top-row right-col bot-row)
             = (iter parse-instructions
                 (for word in (pick-elements (remove "turn" (s:words raw-line) :test #'string=) 0 1 2 4 5))
                 (collect (if (some #'digit-char-p word)
                              (parse-integer word)
                              word))))
        (for (values times rem) = (floor (- right-col left-col) 16))
        (case (schar action 1)
          (#\n (dispatch-action (simd:u8.16-broadcast 1) 1))
          (#\f (dispatch-action (simd:u8.16-broadcast 0) 0))
          (#\o (dispatch-action (simd:u8.16-xor (simd:u8.16-aref lights row col)
                                                (simd:u8.16-broadcast 1))
                                (logxor (aref lights row tail-col) 1))))))
    (values (count 1 dlights)
            lights)))

(declaim (inline s8.16-max))
(defun s8.16-max (pack-1 pack-2)
  #+arm64 (simd:s8.16-bit-select (simd:s8.16< pack-1 pack-2)
                                   pack-2
                                   pack-1)
  #+x86-64 (simd:s8.16-if (simd:s8.16< pack-1 pack-2)
                          pack-2
                          pack-1))

(defun day-6-p2-simd (input-lines)
  (let* ((lights (make-array '(1000 1000) :element-type 'simd:s8 :initial-element (simd:s8 0)))
         (dlights (make-array (array-total-size lights) :element-type 'simd:s8 :displaced-to lights)))
    (macrolet ((dispatch-action (simd-action scalar-action)
                 `(iter grid-rows
                    (for row from top-row to bot-row)
                    (iter grid-cols
                      (for col from left-col by 16)
                      (repeat times)
                      (setf (simd:s8.16-aref lights row col)
                            ,simd-action)
                      (finally (iter tail
                                 (for tail-col from col)
                                 (repeat (1+ rem))
                                 (setf (aref lights row tail-col)
                                       ,scalar-action)))))))
      (iter instructions
        (for raw-line in input-lines)
        (for (action left-col top-row right-col bot-row)
             = (iter parse-instructions
                 (for word in (pick-elements (remove "turn" (s:words raw-line) :test #'string=) 0 1 2 4 5))
                 (collect (if (some #'digit-char-p word)
                              (parse-integer word)
                              word))))
        (for (values times rem) = (floor (- right-col left-col) 16))
        (case (schar action 1)
          (#\n (dispatch-action (simd:s8.16+ (simd:s8.16-aref lights row col) 1)
                                (+ (aref lights row tail-col) 1)))
          (#\f (dispatch-action (s8.16-max (simd:s8.16- (simd:s8.16-aref lights row col) 1) 0)
                                (max (- (aref lights row tail-col) 1) 0)))
          (#\o (dispatch-action (simd:s8.16+ (simd:s8.16-aref lights row col) 2)
                                (+ (aref lights row tail-col) 2))))))
    (values (reduce #'+ dlights);(simd-sum dlights)
            (reduce #'max dlights)
            dlights)))

;; (defun simd-sum (arr &aux (width 16))
;;   (multiple-value-bind (times tail)
;;       (floor (array-total-size arr) width)
;;     (iter
;;       (with acc = (simd:s 0))
;;       (for i from 0 by width)
;;       (repeat times)
;;       (simd:s8.16-incf acc (simd:s8.16-aref arr i))
;;       (finally (+ (reduce #'+ arr :start i)
;;                   (simd:s8.16-horizontal+)q)))))

(defun day-6-time (&aux (in (uiop:read-file-lines *day6input*)))
  (fresh-line)
  (time (princ (day-6-p1 in)))
  (fresh-line)
  (time (princ (day-6-p1-simd in)))
  (fresh-line)
  (time (princ (day-6-p2 in)))
  (fresh-line)
  (time (princ (day-6-p2-simd in))))
