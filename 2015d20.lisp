;;;2015 day 20

(in-package :aoc-2015)


;; There are infinitely many Elves, numbered starting with 1. Each Elf delivers presents equal to ten times his or her number at each house.
;; What is the lowest house number of the house to get at least as many presents as the number in your puzzle input?
;; full input: 34_000_000

;; actual target 3_400_000 (10 presents per elf number)
;; presents recieved must be >= target
;; target = (house number) + (other elves) + 1
;; target - other = house + 1
;; NO if house number is ODD -> presents are sum of prime factors + 1
;; NO if house number is EVEN -> presents are sum of prime factors + 1 + house number
;; new target = target - 1
;; new target = 3_399_999
;; new target is divisible by 3 -> house > 3!
;; target3 = 1_133_333 = (hn + oe)/3
;; cant just do primes - 20 and 100 are missing 10!
;; squares will have duplicated factors
;; sum of all divisors / 100000 = target / 100000 = 34 (ok? does this mean anything?)
;; sum of divisors of 1_000_000 = 2_480_437
;; sum of divisors of target (3_400_000) = 8_929_116
;; between those 2 numbers?
;; no - 1000080 (lowest house number between 1e6 and 3.4e6 giving presents = 3_452_160) is too big
;; #1 reddit solution: https://www.reddit.com/r/adventofcode/comments/3xjpp2/day_20_solutions/cy59zd9/
;; takes problem literally, allocates 1D array of (target) elements and iterates through them and adding all
;; gives 786240,
;; Evaluation took:
;; 0.028 seconds of real time
;; 0.000000 seconds of total run time (0.000000 user, 0.000000 system)
;; 0.00% CPU
;; 55,556,829 processor cycles
;; 8,000,016 bytes consed
;; second algorithm in same comment just does straight iteration, says it is really slow
;; but seems to be just as fast here, but gives different answer - 3248 



(defun collect-divisors (number)
  (let (divisors)
    (dotimes (d (floor number 2) divisors)
      (multiple-value-bind (q r)
          (floor number (1+ d))
        (when (zerop r)
          (a:nunionf divisors (delete-duplicates (list q (1+ d)))))))))

(defun presents-to-house (presents &optional (start 1) (end (expt 10 9)))
  (loop :for house-number :from start :to end
        :when (>= (reduce #'+ (collect-divisors house-number)) presents)
          :return house-number))

(defun sum-divisors-1 (number &optional (end (floor number 2)))
  (loop :with sum fixnum := 0
        :for i :from 1 :to end
        :do (loop :for j :from i :to end :by i
                  :when (zerop (mod i j))
                    :do (incf sum j))
        :when (>= sum number)
          :return (values i sum)))

(defun sum-divisors-2 (number &optional (end (floor number 2)))
   (let ((sum 0))
    (dotimes (i end)
      (declare (fixnum sum i))
      (dotimes (j i)
        (declare (fixnum j))
        (handler-case
            (when (zerop (mod i j))
              (incf sum j))
          (division-by-zero nil nil)))
      (when (>= sum number)
        (return (values i sum))))))

;; all three methods give different answers!

(defday 20
  :test-input ""
  :parse ()
  :p1 ((loop :with a := (make-array (floor (the fixnum input) 2) :element-type 'fixnum)
             :for i fixnum :from 1 :below (array-total-size a)
             :do (loop :for j fixnum :from i :below (array-total-size a) :by i
                       :do (incf (aref a j) i))
             :finally (return (let ((house-number (position-if (lambda (p) (<= input p)) a)))
                                (values house-number (aref a house-number))))))
  :p2 ())
