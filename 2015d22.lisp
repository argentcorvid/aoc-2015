;;;2015 day 22

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
  (player (apply #'make-entity *d22player-initial*))
  (enemy (apply #'make-entity *d22boss-initial*))
  (total-cost 0)
  (best-cost most-positive-fixnum)
  (path (list))
  (edges (copy-list *d22spells*))
  (effects (list)))

(defun d22-spell-name (spell)
  (getf spell :name))

(defun d22-transition-state (state)
  (let-match (((d22-game-state :player (entity :hp player-hp :mp player-mp)
                               :enemy (entity :hp enemy-hp :atk enemy-atk)
                               total-cost
                               best-cost
                               path
                               edges
                               effects)
               state))
    (do-effects)
    ;;player turn
    (let (states-out)
      (dolist (spell *d22spells*)
        (let ((spell-cost (getf spell :cost)))
          (unless (or (>= player-mp spell-cost)
                      (find spell effects :test #'equalp :key #'second))

            (when (plusp new-hp)
              (push new-state states-out))))))
    (do-effects) ;; move up
    (enemy-turn) ;; move up
    ))

(defun a-star (start-state &key edges-func (end-state-pred #'endp) (cost-func #'identity))
  (let ((pqueue (s:make-heap :test #'<= :key cost-func))
        (todo (s:dict 'equalp start-state t))
        (done (s:dict 'equalp)))
    (s:do-hash-table (state v todo)
      (let ((current (s:heap-extract-maximum pqueue)))
        (when (funcall end-state-pred current)
          (return-from a-star current))
        (remhash current todo)
        (setf (gethash current done) t)
        (s:do-each (edge (funcall edges-func current))
          (unless (or (gethash edge done)
                      (gethash edge todo))
            (setf (gethash edge todo) t)
            (s:heap-insert pqueue edge)))))))

(defday 22
  :test-input ""
  :parse ()
  :p1 ()
  :p2 ())
