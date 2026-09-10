;;;2015 day 22

(in-package :aoc-2015)

(defparameter *d22boss-initial*
  (list :hp 58 :atk 9 :effect (list))
  "Hit Points: 58
Damage: 9")

(defparameter *d22player-initial*
  (list :hp 50 :mp 500 :effect (list)))

(defparameter *d22spells*
  (list :magic-missile
          (list :cost 53 :inst (list 4 0))
        :drain
          (list :cost 73 :inst (list 2 2))
        :shield
          (list :cost 113 :effect (list :dur 6 :def 7))
        :poison
          (list :cost 173 :effect (list :dur 6 :dmg 3))
        :recharge
          (list :cost 229 :effecr (list :dur 5 :mp 101))))

(defday 22
  :test-input ""
  :parse ()
  :p1 ()
  :p2 ())
