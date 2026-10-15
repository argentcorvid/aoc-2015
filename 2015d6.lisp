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
    (iter instructions
      (for raw-line in input-lines)
      (for (action left-col top-row right-col bot-row)
           = (iter parse-instructions
               (for word in (pick-elements (remove "turn" (s:words raw-line) :test #'string=) 0 1 2 4 5))
               (collect (if (some #'digit-char-p word)
                            (parse-integer word)
                            word))))
      (for (values times rem) = (floor (- right-col left-col) 16))
      (iter grid-rows
        (for row from top-row to bot-row)
        (case (schar action 1)
          (#\n (iter columns-set
                 (for col from left-col by 16)
                 (repeat times)
                 (setf (simd:u8.16-aref lights row col)
                       (simd:u8.16-broadcast 1))
                 (finally
                  (iter set-tail
                    (for tail-col from col)
                    (repeat rem)
                    (setf (aref lights row tail-col)
                          1)))))
          (#\f (iter columns-clr
                 (for col from left-col by 16)
                 (repeat times)
                 (setf (simd:u8.16-aref lights row col)
                       (simd:u8.16-broadcast 0))
                 (finally
                  (iter clr-tail
                    (for tail-col from col)
                    (repeat rem)
                    (setf (aref lights row tail-col)
                          0)))))
          (#\o (iter columns-tgl
                 (for col from left-col by 16)
                 (repeat times)
                 (setf (simd:u8.16-aref lights row col)
                       (simd:u8.16-xor (simd:u8.16-aref lights row col)
                                       (simd:u8.16-broadcast 1)))
                 (finally
                  (iter tgl-tail
                    (for tail-col from col)
                    (repeat rem)
                    (setf (aref lights row tail-col)
                          (logxor (aref lights row tail-col) 1)))))))))
    (count 1 dlights)))
