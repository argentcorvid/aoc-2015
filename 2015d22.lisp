;;;2015 day 22
;;;https://www.reddit.com/r/adventofcode/comments/3xspyl/day_22_solutions/cy927kk/

(in-package :aoc-2015)

(defparameter *d22boss-initial*
  (list :hp 58 :atk 9 )
  "Hit Points: 58
Damage: 9")

(defparameter *d22player-initial*
  (list :hp 50 :mp 500))

(defparameter *d22spells*
  (list (list :name :magic-missile :cost 53 :inst (list :atk 4 :hp 0) :effect nil)
        (list :name :drain :cost 73 :inst (list :atk 2 :hp 2) :effect nil)
        (list :name :shield :cost 113 :effect (list :dur 6 :def 7) :inst nil)
        (list :name :poison :cost 173 :effect (list :dur 6 :atk 3) :inst nil)
        (list :name :recharge :cost 229 :effect (list :dur 5 :mp 101) :inst nil)))



(defstruct d22-entity
  (hp 0)
  (mp 0)
  (atk 0)
  (def 0))

(defstruct d22-game-state
  (player (apply #'make-d22-entity *d22player-initial*))
  (enemy (apply #'make-d22-entity *d22boss-initial*))
  (total-cost 0)
  (best-cost most-positive-fixnum)
  (path (list))
 ; (edges (copy-list *d22spells*))
  (effects (list)))

(defun d22-spell-name (spell)
  (getf spell :name))

(defun d22-transition-state (state)
  (let-match (((d22-game-state :player (d22-entity :hp player-hp :mp player-mp)
                               :enemy (d22-entity :hp enemy-hp :atk enemy-atk)
                               total-cost
                               best-cost
                               path
                               effects)
               state))
    (labels ((do-effects ()
             (let ((new-effects (list))
                   (new-def 0)
                   (new-hp player-hp) 
                   (new-mp player-mp)
                   (new-enemy-hp enemy-hp))
               (dolist (effect effects)
                 (incf new-hp (getf effect :hp 0))
                 (incf new-mp (getf effect :mp 0))
                 (decf new-enemy-hp (getf effect :atk 0))
                 (a:maxf new-def (getf effect :def 0))
                 (when (> (getf effect :dur) 1)
                   (let ((new-effect (copy-list effect)))
                     (decf (getf new-effect :dur))
                     (push new-effect new-effects))))
               (values new-effects
                       new-hp
                       new-mp
                       new-enemy-hp
                       new-def))))
      (let ((new-player (make-d22-entity :hp player-hp :mp player-mp))
            (new-enemy (make-d22-entity :hp enemy-hp :atk enemy-atk))
            states-out
            new-effects
            new-def)
        (setf (values new-effects
                      (d22-entity-hp new-player)
                      (d22-entity-mp new-player)
                      (d22-entity-hp new-enemy)
                      new-def)
              (do-effects))
        (dolist (spell *d22spells*)
          (let ((spell-cost (getf spell :cost)))
            (unless (or (>= player-mp spell-cost)
                        (find spell effects :key #'second))

              (when (plusp new-hp)
                (push new-state states-out)))))
        (do-effects) ;; move up

        (enemy-turn)) ;; move up
      )))

(defun a-star (start-state &key neighbors-func (end-state-pred #'endp) (cost-func #'identity))
  (let ((pqueue (s:make-heap :test #'<= :key cost-func))
        (done (s:dict 'equalp)))
    (s:heap-insert pqueue start-state)
    (loop (when (zerop (length (s::heap-vector pqueue)))
            (return))
          (let ((current (s:heap-extract-maximum pqueue)))
            (when (funcall end-state-pred current)
              (return-from a-star current))
            (setf (gethash current done) t)
            (s:do-each (neighbor (funcall neighbors-func current))
              (unless (or (gethash neighbor done)
                          (find neighbor (s::heap-vector pqueue) :test #'equalp)
                          )
                (s:heap-insert pqueue neighbor)))))))

(defday 22
  :test-input ""
  :parse ()
  :p1 ()
  :p2 ())
