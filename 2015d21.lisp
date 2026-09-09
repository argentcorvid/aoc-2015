;;;2015 day 21

(in-package :aoc-2015)

(defparameter *d21shopfile* #p"2015d21shop.txt")

(defparameter *day21input* #p"2015d21input.txt")

(defun eqp-split (line)
  (let* ((p (position #\space line))
         (name (subseq line 0 p))
         (rem  (subseq line p)))
    (nconc (list name)
           (mapcar #'parse-integer (str:words rem)))))

(defparameter *d21shop*
  (destructuring-bind (weps arm rings)
      (mapcar #'str:lines
              (str:paragraphs
               (uiop:read-file-string *d21shopfile*)))
    (list (mapcar #'eqp-split (rest weps))
          (mapcar #'eqp-split  (rest arm))
          (mapcar (lambda (line)
                    (let ((p (+ 2 (position #\+ line))))
                      (nconc (list (subseq line 0 p))
                             (mapcar #'parse-integer (str:words (subseq line p))))))
                  (rest rings)))))

(defun d21-read-enemy ()
  (destructuring-bind ((w1 hp)
                       (w2 atk)
                       (w3 arm))
      (mapcar (a:curry #'str:split ": ")
              (uiop:read-file-lines *day21input*))
    (declare (ignore w1 w2 w3))
    (mapcar #'parse-integer (list hp atk arm))))

;; (defun d21-turn (attacker defender) ;;dont need this (for part 1 anyway)
;;   (decf (entity-hp defend)
;;         (max 1
;;              (- (entity-atk attacker)
;;                 (entity-arm defender)))))

(defun d21-loadouts (&optional (shop *d21shop*))
  (let ((weps (first shop))
        (arm  (cons nil (second shop)))
        (rings (remove-if-not (a:rcurry #'s:length<= 2)
                              (s:powerset (third shop))))
        out)
    (dolist (w weps)
      (dolist (a arm)
        (dolist (r rings)
          (destructuring-bind (w-name
                               w-cost
                               w-atk
                               w-arm)
              w
            (destructuring-bind (&optional
                                   (a-name "No Armor")
                                   (a-cost 0)
                                   (a-atk 0)
                                   (a-arm 0))
                a
              (destructuring-bind (&optional
                                     ((&optional (r1-name "No Ring")
                                         (r1-cost 0)
                                         (r1-atk 0)
                                         (r1-arm 0)) nil)
                                     ((&optional (r2-name "No Ring")
                                         (r2-cost 0)
                                         (r2-atk 0)
                                         (r2-arm 0)) nil))
                  r
                (push (list (list w-name a-name r1-name r2-name)
                            (+ w-cost a-cost r1-cost r2-cost)
                            (+ w-atk  a-atk  r1-atk  r2-atk)
                            (+ w-arm  a-arm  r1-arm  r2-arm))
                      out)))))))
    (sort out #'< :key #'second)))

(defday 21
  :test-input *d21shop*
  :parse (())
  :p1 ((let ((player (list 100 0 0))
             (enemy (d21-read-enemy))
             (loadouts (d21-loadouts)))
         (loop :for loadout :in loadouts
               :for (names cost atk arm) (list fixnum fixnum fixnum) := loadout
               :when (>= (ceiling (first player) (max 1 (- (second enemy) (+ (third player) arm))))
                         (ceiling (first enemy)  (max 1 (- (+ (second player) atk) (third enemy)))))
                 :return (values cost loadout))))
  :p2 ((let ((player (list 100 0 0))
             (enemy (d21-read-enemy))
             (loadouts (d21-loadouts)))
         (loop :for loadout :in (reverse loadouts)
               :for (names cost atk arm) (list fixnum fixnum fixnum) := loadout
               :when (< (ceiling (first player) (max 1 (- (second enemy) (+ (third player) arm))))
                         (ceiling (first enemy)  (max 1 (- (+ (second player) atk) (third enemy)))))
                 :return (values cost loadout)))))

(defun d21-p1-inline (&optional (shop *d21shop*))
  (let ((weps (first shop))
        (arm  (cons nil (second shop)))
        (rings (remove-if-not (a:rcurry #'s:length<= 2)
                              (s:powerset (third shop))))
        (out most-positive-fixnum))
    (dolist (w weps)
      (dolist (a arm)
        (dolist (r rings)
          (destructuring-bind (w-name
                               w-cost
                               w-atk
                               w-arm)
              w
            (destructuring-bind (&optional
                                   (a-name "No Armor")
                                   (a-cost 0)
                                   (a-atk 0)
                                   (a-arm 0))
                a
              (destructuring-bind (&optional
                                     ((&optional (r1-name "No Ring")
                                         (r1-cost 0)
                                         (r1-atk 0)
                                         (r1-arm 0)) nil)
                                     ((&optional (r2-name "No Ring")
                                         (r2-cost 0)
                                         (r2-atk 0)
                                         (r2-arm 0)) nil))
                  r
                (let ((eqp (list (list w-name a-name r1-name r2-name)
                                 (+ w-cost a-cost r1-cost r2-cost)
                                 (+ w-atk  a-atk  r1-atk  r2-atk)
                                 (+ w-arm  a-arm  r1-arm  r2-arm))))
                  (when (>= (ceiling 100 (max 1 (- 8 (fourth eqp))))
                            (ceiling 104  (max 1 (- (third eqp) 1))))
                    (a:minf out (second eqp))))))))))
    out))

(defun d21-p1-match (&optional (shop *d21shop*))
  (let ((mincost most-positive-fixnum)
        minnames)
    (when-match (list weps arm rings) shop
      (dolist (w weps)
        (dolist (a (cons nil arm))
          (dolist (r (s:filter (a:rcurry #'s:length<= 2)
                               (s:powerset rings)))
            (let-match (((list w-name w-cost w-atk _) w)
                        ((list a-name a-cost _ a-arm) (or a (list "No Armor" 0 0 0)))
                        ((list (list r1-name r1-cost r1-atk r1-arm)
                               (list r2-name r2-cost r2-atk r2-arm))
                         (match r
                           (nil
                            (make-list 2 :initial-element (list "No ring" 0 0 0)))
                           ((list elt)
                            (list elt (list "No Ring" 0 0 0)))
                           (otherwise r) )))
              (when (>= (ceiling 100 (max 1 (- 8 (+ a-arm r1-arm r2-arm))))
                        (ceiling 104  (max 1 (- (+ w-atk r1-atk r2-atk) 1))))
                (let ((c (+ w-cost a-cost r1-cost r2-cost)))
                  (when (< c mincost)
                    (setf mincost c)
                    (setf minnames (list w-name a-name r1-name r2-name))))))))))
    (values mincost minnames)))
