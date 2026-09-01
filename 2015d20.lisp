;;;2015 day 20

(in-package :aoc-2015)


;;There are infinitely many Elves, numbered starting with 1. Each Elf delivers presents equal to ten times his or her number at each house.
;;What is the lowest house number of the house to get at least as many presents as the number in your puzzle input?
;;full input: 34_000_000

;; actual target 3_400_000 (10 presents per elf number)
;; presents recieved must be >= target
;; target = (house number) + (other elves) + 1
;; target - other = house + 1
;; NO if house number is ODD -> presents are sum of prime factors + 1
;; NO if house number is EVEN -> presents are sum of prime factors + 1 + house number
;; new target = target - 1
;; new target = 3_399_999
;; new target is divisible by 3 -> house > 3!
;; target3 = 1_133_333 = (hn + oe + 1)/3

(defparameter *day20primes* (list 2      3      5      7     11     13     17     19     23     29 
     31     37     41     43     47     53     59     61     67     71 
     73     79     83     89     97    101    103    107    109    113 
    127    131    137    139    149    151    157    163    167    173 
    179    181    191    193    197    199    211    223    227    229 
    233    239    241    251    257    263    269    271    277    281 
    283    293    307    311    313    317    331    337    347    349 
    353    359    367    373    379    383    389    397    401    409 
    419    421    431    433    439    443    449    457    461    463 
    467    479    487    491    499    503    509    521    523    541 
    547    557    563    569    571    577    587    593    599    601 
    607    613    617    619    631    641    643    647    653    659 
    661    673    677    683    691    701    709    719    727    733 
    739    743    751    757    761    769    773    787    797    809 
    811    821    823    827    829    839    853    857    859    863 
    877    881    883    887    907    911    919    929    937    941 
    947    953    967    971    977    983    991    997   1009)) ;;not going to be enough - read in from file?

(defun collect-divisors (number)
  (let (divisors)
    (dolist (d (remove-if (a:curry #'<= number) *day20primes*))
      (multiple-value-bind (q r)
          (floor number d)
        (when (zerop r)
          (push (list q d) divisors))))
    (sort (nconc (list 1 number) divisors) #'<))) ;don't need to sort here, will be to slow, do it when printing

(defday 20
  :test-input ""
  :parse ()
  :p1 ()
  :p2 ())
