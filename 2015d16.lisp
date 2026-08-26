;;;2015 day 16

(in-package :aoc-2015)

(defparameter *day16key*
  (apply #'s:dict
         (mapcar (lambda (itm)
                   (if (some #'digit-char-p itm)
                       (parse-integer itm)
                       itm))
                 (s:words
                  "children: 3
cats: 7
samoyeds: 2
pomeranians: 3
akitas: 0
vizslas: 0
goldfish: 5
trees: 3
cars: 2
perfumes: 1"))))

(defday 16
  :parse ((let ((sue-table (s:dict)))
            (ppcre:do-register-groups
                ((#'parse-integer sue-number)
                 k1 (#'parse-integer v1)
                 k2 (#'parse-integer v2)
                 k3 (#'parse-integer v3))
                ("(?m)Sue (\\d{1,}): (\\w{1,}): (\\d{1,}), (\\w{1,}): (\\d{1,}), (\\w{1,}): (\\d{1,})"
                 input
                 sue-table)
              
              (s:addhash v1 sue-number (a:ensure-gethash k1 sue-table (s:dict)))
              (s:addhash v2 sue-number (a:ensure-gethash k2 sue-table (s:dict)))
              (s:addhash v3 sue-number (a:ensure-gethash k3 sue-table (s:dict))))))

  :p1 ((a:map-combinations (lambda (to-lookup)
                             (destructuring-bind (k1 k2 k3) to-lookup
                               (let ((sue-list-1 (s:href input k1 (gethash k1 *day16key*)))
                                     (sue-list-2 (s:href input k2 (gethash k2 *day16key*)))
                                     (sue-list-3 (s:href input k3 (gethash k3 *day16key*))))
                                 (a:when-let ((i1 (intersection sue-list-1 sue-list-2))
                                              (i2 (intersection sue-list-3 sue-list-2))
                                              (i3 (intersection sue-list-1 sue-list-3)))
                                   (return-from day-16-p1 res)))))
                           (a:hash-table-keys *day16key*)
                           :length 3))
  :p2 ())
