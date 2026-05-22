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
  :p2 ((let ((brightness 0)
             (scanner (ppcre:create-scanner "(e|n|f) (\\d{1,}),(\\d{1,}) \\w+ (\\d{1,}),(\\d{1,})")))
         (declare (fixnum brightness))
         (dolist (inst input) 
           (ppcre:register-groups-bind
               (((a:compose #'intern #'string-upcase) action)
                (#'parse-integer c1-x c1-y c2-x c2-y))
               (scanner inst)
             (setf brightness (a:clamp
                               (+ brightness (* (1+ (- c2-x c1-x))
                                                (1+ (- c2-y c1-y))
                                                (case action
                                                  (e 2)
                                                  (n 1)
                                                  (f -1))))
                               0 most-positive-fixnum))))
         brightness)))
