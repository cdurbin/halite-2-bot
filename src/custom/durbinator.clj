(ns custom.durbinator
  (:require
   [clojure.stacktrace :as stack]
   [custom.game-map :refer [*safe-planets* *docked-enemies* *pesky-fighters* *num-ships*
                            *num-players* *start-ms*]]
   [custom.map-analysis :as map]
   [custom.math :as custom-math :refer [infinity]]
   [custom.navigation :as navigation]
   [custom.swarm :as swarm]
   [custom.utils :as utils :refer [defn-timed pretty-log]]
   [custom.center-planet :as center-planet]
   [hlt.entity :as e]
   [hlt.game-map :refer [*player-id* *map-size* *bot-name* *ships* *planets* *owner-ships*]]
   [hlt.math :as math]
   [hlt.utils :refer [log]])
  (:import (java.io PrintWriter)))

(def my-bot-name "Durbinator")

(def all-out-attack
  (atom 0))

; (def close-to-dock-point 5)

(defn move-ship-to-planet!
  "Moves the ship to the given planet. Side effect to update the planet to reduce the number of
  available docking spots by one."
  [ship planet]
  ; (log "Trying to move to planet" planet "from ship" ship)
  (let [docking-point (center-planet/get-best-docking-point ship planet)
        move (if (and (e/within-docking-range? ship planet))
                      ; (< (math/distance-between ship docking-point) close-to-dock-point))
               (do (set! *ships* (assoc-in *ships* [(:id ship) :docking :status] :custom-docking))
                   (e/dock-move ship planet))
               (navigation/navigate-to-dock ship planet docking-point))]
    (when (and move (or (pos? (get move :thrust 0))
                        (= :dock (:type move))))
      (let [upd-planet (update-in planet [:docking :ships] conj ship)]
        (set! *planets* (assoc *planets* (:id planet) upd-planet))
        (if-not (e/any-remaining-docking-spots? upd-planet)
          (set! *safe-planets* (dissoc *safe-planets* (:id planet)))
          (set! *safe-planets* (assoc *safe-planets* (:id planet) upd-planet)))))
    move))

(defn move-ship-to-attack
  "Moves the ship to attack the enemy ship."
  [ship enemy-ship]
  ; (log "I tried to attack a ship???")
  (let [move (navigation/navigate-to-attack-ship ship enemy-ship
                                                 ; false)]
                                                 (> (math/distance-between ship enemy-ship) 21.1))]
    ; (log "Move" move)
    (when (and move (pos? (:thrust move)))
      ; (swap! map/attack-spots conj (custom-math/get-point ship (:thrust move) (:angle move)))
      (map/update-fighters enemy-ship))
      ; (let [fighter? (= :undocked (-> enemy-ship :docking :status))
      ;       attack-count (inc (get enemy-ship :attack-count 0))
      ;       remove? (>= attack-count 4)]
      ;   (if fighter?
      ;     (if remove?
      ;       (set! *pesky-fighters* (dissoc *pesky-fighters* (:id enemy-ship)))
      ;       (set! *pesky-fighters* (assoc-in *pesky-fighters* [(:id enemy-ship) :attack-count] attack-count)))
      ;     (if remove?
      ;       (set! *docked-enemies* (dissoc *docked-enemies* (:id enemy-ship)))
      ;       (set! *docked-enemies* (assoc-in *docked-enemies* [(:id enemy-ship) :attack-count] attack-count))))))
    move))

(def retreat-if-this-close 25)

(defn move-ship-to-retreat
  "Moves the ship to retreat from the enemy ship."
  [ship enemy-ship]
  ; (log "Move ship to retreat")
  ; (let [my-ships (map/get-my-real-ships) ;; TODO should this only be fighters?
  (let [my-ships (map/get-fighters *player-id*) ;; TODO should this only be fighters?
        my-other-ships (remove #(= (:id ship) (:id %))
                               my-ships)
        closest-friendly-ship (map/nearest-entity ship my-other-ships)]
    (if (and closest-friendly-ship
             (< (math/distance-between closest-friendly-ship ship) retreat-if-this-close))
      ; (navigation/navigate-to-friendly-ship-later ship closest-friendly-ship)
      (navigation/navigate-to-friendly-ship-later ship)
      (let [closest-enemy-planet (map/farthest-entity ship (map/get-planets (:owner-id enemy-ship)))]
        (if closest-enemy-planet
          (navigation/navigate-to-retreat ship closest-enemy-planet)
          (navigation/navigate-to-retreat-ship ship enemy-ship))))))

(defn move-ship-to-retreat-for-real
  "Moves the ship to retreat from the enemy ship."
  [ship enemy-ship]
  ; (log "Move ship to retreat fo real")
  (let [closest-enemy-planet (map/farthest-entity ship (map/get-planets (:owner-id enemy-ship)))]
    (if closest-enemy-planet
      (navigation/navigate-to-retreat ship closest-enemy-planet)
      (navigation/navigate-to-retreat-ship ship enemy-ship))))

; (def tag-team-range 5)
(def tag-team-range 9)
(def retreat-range 24.1)

(def retreat-range-early 60)

(def ignore-retreating-ship-count
  "Optimization to not worry about running away when I have this many ships."
  125)

(defn get-retreat-range
  "Returns the retreat range based on number of ships."
  [num-ships]
  (if (> num-ships 3)
    retreat-range
    retreat-range-early))

(defn move-to-nearest-enemy-ship-or-target
  "Moves the ship to the nearest enemy ship."
  [ship enemy-ships target]
  (let [
        ; nemesis (deref map/nemesis)
        ; enemy-ships (if nemesis
        ;               (filter #(= nemesis (:owner-id %)) enemy-ships)
        ;               enemy-ships)
        nearest-docked-enemy-ship (map/nearest-entity
                                   ship (vals (deref map/top-player-docked-ships)))
        enemy-ship (map/nearest-enemy-not-decoy ship enemy-ships)]
    (when enemy-ship
      (if (> *num-ships* 3)
        (move-ship-to-attack ship (or nearest-docked-enemy-ship enemy-ship))
        (let [distance (math/distance-between ship enemy-ship)]
          (if (> distance (get-retreat-range *num-ships*))
            (move-ship-to-attack ship enemy-ship)
            (let [attack? (map/have-advantage? (custom-math/get-closest-point-towards-target ship enemy-ship (- distance 5)))]
              (if attack?
                (move-ship-to-attack ship enemy-ship)
                (if (map/alone? ship enemy-ship *player-id* tag-team-range false)
                  (move-ship-to-retreat ship enemy-ship)
                  (navigation/navigate-to-retreat ship target))))))))))

; (defn move-to-nearest-enemy-ship-or-target
;   "Moves the ship to the nearest enemy ship."
;   [ship enemy-ships target]
;   (when-let [enemy-ship (map/nearest-enemy-not-decoy ship enemy-ships)]
;     (if (> *num-ships* ignore-retreating-ship-count)
;       (move-ship-to-attack ship enemy-ship)
;       (let [distance (math/distance-between ship enemy-ship)]
;         (if (> distance (get-retreat-range *num-ships*))
;           (move-ship-to-attack ship enemy-ship)
;           ; (let [attack? (map/have-advantage? (custom-math/get-point-between ship enemy-ship 0.8))]
;           ; (let [attack? (map/have-advantage? enemy-ship)]
;           ; (let [attack? (map/have-advantage? (custom-math/get-closest-point-towards-target ship enemy-ship (min 7 (- distance 3))))])
;           (let [attack? (map/have-advantage? (custom-math/get-closest-point-towards-target ship enemy-ship (- distance 5)))]
;
;           ; (let [attack? false]
;             (if attack?
;               ; (let [nearest-ship (nearest-ship)])
;               (move-ship-to-attack ship enemy-ship)
;               (move-ship-to-attack ship enemy-ship))))))))
;               ; (if (map/alone? ship enemy-ship *player-id* tag-team-range false)
;               ;   (move-ship-to-retreat ship enemy-ship)
;               ;   (navigation/navigate-to-retreat ship target)))))))))

; (defn move-to-nearest-enemy-ship
;   "Moves the ship to the nearest enemy ship."
;   [ship enemy-ships]
;   (when-let [enemy-ship (map/nearest-enemy-not-decoy ship enemy-ships)]
;     (if (> *num-ships* 3)
;       (move-ship-to-attack ship enemy-ship)
;       (let [distance (math/distance-between ship enemy-ship)]
;         (if (> distance (get-retreat-range *num-ships*))
;           (move-ship-to-attack ship enemy-ship)
;           (let [attack? (map/have-advantage? (custom-math/get-closest-point-towards-target ship enemy-ship (- distance 5)))]
;             (if attack?
;               (move-ship-to-attack ship enemy-ship)
;               (if (map/alone? ship enemy-ship *player-id* tag-team-range false)
;                 (move-ship-to-retreat ship enemy-ship)
;                 (move-ship-to-retreat-for-real ship enemy-ship)))))))))

(defn move-to-nearest-enemy-ship
  "Moves the ship to the nearest enemy ship."
  [ship enemy-ships]
  (let [
        ; nemesis (deref map/nemesis)
        ; enemy-ships (if nemesis
        ;               (filter #(= nemesis (:owner-id %)) enemy-ships)
        ;               enemy-ships)
        nearest-docked-enemy-ship (map/nearest-entity
                                   ship (vals (deref map/top-player-docked-ships)))
        enemy-ship (map/nearest-enemy-not-decoy ship enemy-ships)]
    (when enemy-ship
      (if (> *num-ships* 0)
      ; (if (> *num-ships* 3)
        (move-ship-to-attack ship (or nearest-docked-enemy-ship enemy-ship))
        (let [distance (math/distance-between ship enemy-ship)]
          (if (> distance (get-retreat-range *num-ships*))
            (move-ship-to-attack ship enemy-ship)
            (let [attack? (map/have-advantage? (custom-math/get-closest-point-towards-target ship enemy-ship (- distance 5)))]
              (if attack?
                (move-ship-to-attack ship enemy-ship)
                (if (map/alone? ship enemy-ship *player-id* tag-team-range false)
                  (move-ship-to-retreat ship enemy-ship)
                  (move-ship-to-retreat-for-real ship enemy-ship))))))))))

; (defn move-to-nearest-enemy-ship
;   "Moves the ship to the nearest enemy ship."
;   [ship enemy-ships]
;   (when-let [enemy-ship (map/nearest-enemy-not-decoy ship enemy-ships)]
;     (if (> *num-ships* ignore-retreating-ship-count)
;       (move-ship-to-attack ship enemy-ship)
;       (let [distance (math/distance-between ship enemy-ship)]
;         (if (> distance (get-retreat-range *num-ships*))
;           (move-ship-to-attack ship enemy-ship)
;           ; (let [attack? (map/have-advantage? (custom-math/get-point-between ship enemy-ship 0.8))]
;           ; (let [attack? (map/have-advantage? enemy-ship)])
;           ; (let [attack? (map/have-advantage? (custom-math/get-closest-point-towards-target ship enemy-ship (min 7 (- distance 3))))])
;           (let [attack? (map/have-advantage? (custom-math/get-closest-point-towards-target ship enemy-ship (- distance 5)))]
;
;           ; (let [attack? false]
;             (if attack?
;               (move-ship-to-attack ship enemy-ship)
;               (move-ship-to-attack ship enemy-ship))))))))
;               ; (if (map/alone? ship enemy-ship *player-id* tag-team-range false)
;               ;   (move-ship-to-retreat ship enemy-ship)
;               ;   (move-ship-to-retreat-for-real ship enemy-ship)))))))))

(defn get-reachable-attack-spot-move
  "Figures out the move to make in order to move to an attack spot."
  [ship]
  nil)
  ; (when-let [best-spot (map/get-best-attack-spot ship)]
  ;   (log "I am trying to move to best spot" best-spot)
  ;   (let [move (navigation/navigate-to-friendly-ship ship best-spot)]
  ;     (when (and move (pos? (:thrust move)))
  ;       move))))

; (defn compute-move-closest-planet*
;   "Picks the move for the ship based on proximity to planets and fighters near planets."
;   [{:keys [start-ms]} ship]
;   (let [times-up? (> (- (System/currentTimeMillis) *start-ms*) 1625)]
;     (if (or times-up?
;             (not= :undocked (-> ship :docking :status)))
;       nil
;       (let [move (get-reachable-attack-spot-move ship)]
;         (if move
;           move
;           (if (pos? @all-out-attack)
;             (move-to-nearest-enemy-ship ship (concat (vals *pesky-fighters*) (vals *docked-enemies*)))
;
;             (when-let [planets (filter #(or (not= *player-id* (:owner-id %))
;                                             (e/any-remaining-docking-spots? %))
;                                        (vals *planets*))]
;               (let [nearest-planet
;                     (:nearest-planet
;                      (reduce (fn [{:keys [min-distance nearest-planet]} planet]
;                                (let [distance-to-planet (- (math/distance-between ship planet) (:radius planet))]
;                                  (if (< distance-to-planet min-distance)
;                                    {:min-distance distance-to-planet :nearest-planet planet}
;                                    {:min-distance min-distance :nearest-planet nearest-planet})))
;                              {:min-distance infinity}
;                              planets))]
;                 (if (and nearest-planet
;                          (some #{(:id nearest-planet)} (keys *safe-planets*))
;                          ; No fighters close to taking me
;                          (map/safe-to-dock? ship))
;                   (move-ship-to-planet! ship nearest-planet)
;                   (move-to-nearest-enemy-ship ship (concat (vals *pesky-fighters*) (vals *docked-enemies*))))))))))))
;
; (defn compute-move-closest-planet
;   "Picks the move for the ship based on proximity to planets and fighters near planets."
;   [{:keys [start-ms moving-ships] :as custom-map-info} ship]
;   (when-not (some #{(:id ship)} moving-ships)
;     (when-let [move (compute-move-closest-planet* custom-map-info ship)]
;       (map/change-ship-positions! move)
;       move)))

; (defn compute-move-best-planet*
;   "Picks the move for the ship based on proximity to planets and fighters near planets."
;   [{:keys [start-ms]} ship]
;   (let [times-up? (> (- (System/currentTimeMillis) *start-ms*) 1625)]
;     (if (or times-up?
;             (not= :undocked (-> ship :docking :status)))
;       nil
;       (if (pos? @all-out-attack)
;         (move-to-nearest-enemy-ship ship (concat (vals *pesky-fighters*) (vals *docked-enemies*)))
;
;         (when-let [planets (filter #(or (not= *player-id* (:owner-id %))
;                                         (e/any-remaining-docking-spots? %))
;                                    (vals *planets*))]
;           (let [best-planet
;                 (:nearest-planet
;                  (reduce (fn [{:keys [min-distance nearest-planet]} planet]
;                            (let [distance-to-planet (- (math/square (- (math/distance-between ship planet)
;                                                                        (:radius planet)))
;                                                        (get planet :square-value 0))]
;                              (if (< distance-to-planet min-distance)
;                                {:min-distance distance-to-planet :nearest-planet planet}
;                                {:min-distance min-distance :nearest-planet nearest-planet})))
;                          {:min-distance infinity}
;                          planets))]
;             (if (and best-planet
;                      (some #{(:id best-planet)} (keys *safe-planets*))
;                      ; No fighters close to taking me
;                      (map/safe-to-dock? ship))
;               (move-ship-to-planet! ship best-planet)
;               (move-to-nearest-enemy-ship ship (concat (vals *pesky-fighters*) (vals *docked-enemies*))))))))))

; (defn compute-move-best-planet
;   "Picks the move for the ship based on proximity to planets and fighters near planets."
;   [{:keys [start-ms moving-ships] :as custom-map-info} ship]
;   (when-not (some #{(:id ship)} moving-ships)
;     (when-let [move (compute-move-best-planet* custom-map-info ship)]
;       (map/change-ship-positions! move)
;       move)))

(def unprotected-distance
  "How far away we look for unprotected ships."
  32)

(def early-unprotected-distance 56)

(defn get-unprotected-distance
  "Returns the distance to look for unprotected ships depending on the number of ships."
  [num-ships]
  (if (< num-ships 6)
    early-unprotected-distance
    unprotected-distance))

(defn- find-unprotected-ships-old
  "Helper function."
  [potential-ships assigned-ships]
  (for [enemy-ship (vals *docked-enemies*)
        :let [no-help (nil? (second (filter #(and (= (:owner-id enemy-ship (:owner-id %)))
                                                  (<= (math/distance-between % enemy-ship) 5))
                                            potential-ships)))]
        ; :let [no-help (navigation/unreachable? enemy-ship (filter #(= (:owner-id enemy-ship)
        ;                                                               (:owner-id %))
        ;                                                           potential-ships))]
        :when no-help
        :let [closest-ship (map/nearest-entity enemy-ship
                                               (remove #(or (= (:id enemy-ship) (:id %))
                                                            ; (not= *player-id* (:id %))
                                                            (some (set [%]) @assigned-ships))
                                                       potential-ships))]
        :when (and closest-ship
                   (= *player-id* (:owner-id closest-ship))
                   (not (some #{closest-ship} @assigned-ships)))
        :let [distance (math/distance-between enemy-ship closest-ship)
              turns (int (/ distance 7))
              turns-to-new-ship (map/get-turns-to-new-ship enemy-ship)
              _ (log "Turns" turns "new ship" turns-to-new-ship)]
        :when (<= turns turns-to-new-ship)]
    (do
      (log "I found an unprotected ship! They will spawn a new ship in" turns-to-new-ship
           "I get there in " turns)
      (swap! assigned-ships conj closest-ship)
      {:ship closest-ship
       :enemy-ship enemy-ship
       :distance distance})))

(defn unprotected-enemy-ships-old
  "Returns a map of unprotected enemy ships with my ship as the key and the enemy ship as the value.
  TODO - this will only find a single dock point most of the time because the same ship of mine
  is probably the closest to several spots. Remove my ship each time it is selected. Will require
  several passes."
  [moving-ships]
  (let [distance unprotected-distance
        potential-ships (filter #(and (= :undocked (-> % :docking :status))
                                      (not (some (set [(:id %)]) moving-ships)))
                                (vals *ships*))
        assigned-ships (atom nil)
        all-permutations (find-unprotected-ships-old potential-ships assigned-ships)
        sorted-order (sort (utils/compare-by :distance utils/desc) all-permutations)]
    ;; This makes sure that if the same ship is the closest to multiple it picks the closest one.
    (into {}
      (for [triple-map sorted-order]
        [(:ship triple-map) (:enemy-ship triple-map)]))))

; (defn attack-unprotected-enemy-ships
;   "Returns moves to attack the unprotected enemy ships"
;   [moving-ships {:keys [start-ms]}]
;   [])

(defn attack-unprotected-enemy-ships-old
  "Returns moves to attack the unprotected enemy ships"
  [moving-ships {:keys [start-ms]}]
  (log "Planets" (pretty-log (vals *planets*)))
  (if (< *num-ships* 200)
    (let [ship-attacks (unprotected-enemy-ships-old moving-ships)]
      ; (log "unprotected enemy ships are:" ship-attacks)
      (doall
       (for [[ship enemy-ship] ship-attacks
             :let [times-up? (> (- (System/currentTimeMillis) *start-ms*) 1625)]
             :when (not times-up?)
             :let [move (navigation/navigate-to-attack-docked-ship
                         ship enemy-ship true)]
                         ; ship enemy-ship (> (math/distance-between ship enemy-ship) 21.1)
                         ; ship enemy-ship (> (math/distance-between ship enemy-ship) 12))]
             :when move]
         (do (map/change-ship-positions! move)
             move))))))

(defn- find-unprotected-ships
  "Helper function."
  [potential-ships]
  (let [counter (atom 0)]
    (into {}
      (for [enemy-ship (vals *docked-enemies*)
            :let [other-fighters (remove #(= (:id enemy-ship) (:id %)) potential-ships)
            ;       no-help (nil? (first (filter #(and (= (:owner-id enemy-ship (:owner-id %)))
            ;                                          (not= (:id enemy-ship) (:id %))
            ;                                          (<= (math/distance-between % enemy-ship) 5))
            ;                                    other-fighters)))]
            ; :when no-help
            ; :let [turns-to-new-ship (map/get-turns-to-new-ship enemy-ship)]
                  turns-to-new-ship (map/get-turns-to-new-ship enemy-ship)
                  in-range-ships (filter #(<= (math/distance-between % enemy-ship)
                                              (* 7 turns-to-new-ship))
                                         other-fighters)
                  my-ships (filter #(= *player-id* (:owner-id %)) in-range-ships)
                  my-ship-count (count my-ships)
                  other-ship-count (- (count in-range-ships) my-ship-count)]
            :when (> my-ship-count other-ship-count)
            :let [attack-ships (for [ship my-ships
                                     :let [distance (math/distance-between enemy-ship ship)]]
                                 {:ship ship :distance distance})]]
        (do
          (let [id (swap! counter inc)
                attack-group [id {:attack-ships attack-ships
                                  :enemy-docked-ship enemy-ship
                                  :num-defenders other-ship-count
                                  :turns turns-to-new-ship
                                  :id id}]]
            (log "I found an unprotected ship. I will attack with" attack-group)
            attack-group))))))

(defn process-attack-groups
  "Takes a list of attack groups to figure out how best to attack.
  Here's the tricky part - we want to assign attack groups in a way that we have enough
  ships to take out the docked ship, while not counting the same ship in multiple groups
  If we always assign groups based on the closest distance we might end up in a place where
  we assign attack groups to the closest places only to find we can no longer attack either
  group because we removed the critical ship that put us over the top. :(
  So the plan is choose attack groups with the least number of turns first (time critical)
  Make sure we have at least one extra attacker in that group. Once we have enough
  attackers remove those chosen ones (the closest ones) from every other attack group.
  Go to the next attack group and do the same for each.
  If an attack group no longer has the advantage remove that attack group.
  Then in reverse order remove any ship that belongs to a higher priority attack group
  If we have more than 3 more than the number of defenders, remove them as well
  Information needed:
  Attack group {:attack-ships [] :enemy-docked-ship :num-defenders :turns-to-new-ship}
  The attack-ships need the following - :ship :distance (TBD maybe health though really
  unlikely to be important)"
  [attack-group-map]
  (let [sorted-attack-groups (sort (utils/compare-by :turns utils/asc :num-defenders utils/asc)
                                   (vals attack-group-map))
        ;; TODO figure out a better way for mutation - maybe reduce
        final-attack-group-map (atom attack-group-map)]
    (log "CDD: Starting sorted-attack-groups" (pretty-log sorted-attack-groups))
    (log "CDD: Starting attack-group-map" (pretty-log @final-attack-group-map))
    ;; Go through each one and remove fighters from the other attack groups as they are locked
    (doseq [attack-group sorted-attack-groups
            :let [up-to-date-attack-group (get @final-attack-group-map (:id attack-group))
                  {:keys [attack-ships enemy-docked-ship num-defenders]} up-to-date-attack-group
                  attack-ships (map :ship (sort (utils/compare-by :distance utils/asc) attack-ships))
                  locked-defenders (take (inc num-defenders) attack-ships)]
            :when (> (count attack-ships) num-defenders)
            locked-ship locked-defenders
            final-group sorted-attack-groups
            :when (not= (:id final-group) (:id attack-group))]
      ;; Remove the locked defenders from every other group
      (do
        (reset! final-attack-group-map (update-in @final-attack-group-map
                                                  [(:id final-group) :attack-ships]
                                                  (fn [old-value]
                                                    (remove #(= (:id locked-ship) (:id (:ship %)))
                                                            old-value))))))

    (doseq [attack-group sorted-attack-groups
            :let [up-to-date-attack-group (get @final-attack-group-map (:id attack-group))
                  {:keys [attack-ships enemy-docked-ship num-defenders]} up-to-date-attack-group
                  attack-ships (map :ship (sort (utils/compare-by :distance utils/asc) attack-ships))]
            locked-ship attack-ships
            final-group sorted-attack-groups
            :when (not= (:id final-group) (:id attack-group))]
      ;; Remove the locked defenders from every other group
      (do
        (log "CDD: I am trying to remove ship " locked-ship "from all other groups")
        (log "I'm checking for ID" (:id locked-ship) "in this group of ships" (get-in @final-attack-group-map [(:id final-group) :attack-ships]))
        (reset! final-attack-group-map (update-in @final-attack-group-map
                                                  [(:id final-group) :attack-ships]
                                                  (fn [old-value]
                                                    (log "the old-value is" old-value)
                                                    (log "ID of locked-ship" (:id locked-ship))
                                                    (log "ID of other ships" (mapv #(:id (:ship %)) old-value))
                                                    (remove #(= (:id locked-ship) (:id (:ship %)))
                                                            old-value))))))

    (log "CDD: Processed locked defenders:" (pretty-log @final-attack-group-map))
    (for [attack-group (vals @final-attack-group-map)
          :when (> (count (:attack-ships attack-group)) (:num-defenders attack-group))]
      (do
          ;(log "Now my final attack groups are" attack-group)
          ;; Attack with no more than 5 extra than they have defenders
          (update-in attack-group [:attack-ships] (fn [old-attacks]
                                                    (take (+ 5 (:num-defenders attack-group)) old-attacks)))))))

(defn create-moves-for-attack-groups
  "This assumes the attack group has already been processed down. Creates the moves based on the
  attack group map"
  [attack-groups]
  (log "CDD: creating moves for attack groups " attack-groups)
  (for [{:keys [attack-ships enemy-docked-ship attack-groups]} attack-groups
        my-ship attack-ships
        :let [my-ship (:ship my-ship)
              times-up? (> (- (System/currentTimeMillis) *start-ms*) 1625)]
        :when (not times-up?)
        :let [_ (log "CDD: I'm trying to create a move for my-ship" my-ship "to attack enemy" enemy-docked-ship)
              move (navigation/navigate-to-attack-docked-ship
                    my-ship enemy-docked-ship true)]
                    ; ship enemy-ship (> (math/distance-between ship enemy-ship) 21.1)
                    ; ship enemy-ship (> (math/distance-between ship enemy-ship) 12))]
        :when move]
    (do
        (log "CDD: MOVE" move)
        (map/change-ship-positions! move)
        move)))

(defn attack-unprotected-enemy-ships
  "Returns moves to attack the unprotected enemy ships"
  [moving-ships {:keys [start-ms]}]
  ; (log "Planets" (pretty-log (vals *planets*)))
  (let [potential-ships (filter #(and (= :undocked (-> % :docking :status))
                                      (not (some (set [(:id %)]) moving-ships)))
                                (vals *ships*))
        attack-groups (find-unprotected-ships potential-ships)
        processed-attack-groups (process-attack-groups attack-groups)]
    (doall
     (create-moves-for-attack-groups processed-attack-groups))))

; (defn- find-unprotected-ships
;   "Helper function."
;   [potential-ships assigned-ships]
;   (for [enemy-ship (vals *docked-enemies*)
;         :let [no-help (nil? (first (filter #(and (= (:owner-id enemy-ship (:owner-id %)))
;                                                  (not= (:id enemy-ship) (:id %))
;                                                  (<= (math/distance-between % enemy-ship) 5))
;                                            potential-ships)))]
;         :when no-help
;         :let [closest-ship (map/nearest-entity enemy-ship
;                                                (remove #(or (= (:id enemy-ship) (:id %))
;                                                             ; (not= *player-id* (:id %))
;                                                             (some (set [%]) @assigned-ships))
;                                                        potential-ships))]
;         :when (and closest-ship
;                    (= *player-id* (:owner-id closest-ship))
;                    (not (some #{closest-ship} @assigned-ships)))
;         :let [distance (math/distance-between enemy-ship closest-ship)
;               turns (int (/ distance 7))
;               turns-to-new-ship (map/get-turns-to-new-ship enemy-ship)
;               _ (log "Turns" turns "new ship" turns-to-new-ship)]
;         :when (<= turns turns-to-new-ship)]
;     (do
;       (log "I found an unprotected ship! They will spawn a new ship in" turns-to-new-ship
;            "I get there in " turns)
;       (swap! assigned-ships conj closest-ship)
;       {:ship closest-ship
;        :enemy-ship enemy-ship
;        :distance distance})))

; (defn unprotected-enemy-ships
;   "Returns a map of unprotected enemy ships with my ship as the key and the enemy ship as the value.
;   TODO - this will only find a single dock point most of the time because the same ship of mine
;   is probably the closest to several spots. Remove my ship each time it is selected. Will require
;   several passes."
;   [moving-ships]
;   (let [distance unprotected-distance
;         potential-ships (filter #(and (= :undocked (-> % :docking :status))
;                                       (not (some (set [(:id %)]) moving-ships)))
;                                 (vals *ships*))
;         assigned-ships (atom nil)
;         all-permutations (find-unprotected-ships potential-ships assigned-ships)
;         sorted-order (sort (utils/compare-by :distance utils/desc) all-permutations)]
;     ;; This makes sure that if the same ship is the closest to multiple it picks the closest one.
;     (into {}
;       (for [triple-map sorted-order]
;         [(:ship triple-map) (:enemy-ship triple-map)]))))

; (defn attack-unprotected-enemy-ships
;   "Returns moves to attack the unprotected enemy ships"
;   [moving-ships {:keys [start-ms]}]
;   [])

; (defn attack-unprotected-enemy-ships
;   "Returns moves to attack the unprotected enemy ships"
;   [moving-ships {:keys [start-ms]}]
;   (log "Planets" (pretty-log (vals *planets*)))
;   (if (< *num-ships* 200)
;     (let [ship-attacks (unprotected-enemy-ships moving-ships)]
;       ; (log "unprotected enemy ships are:" ship-attacks)
;       (doall
;        (for [[ship enemy-ship] ship-attacks
;              :let [times-up? (> (- (System/currentTimeMillis) *start-ms*) 1625)]
;              :when (not times-up?)
;              :let [move (navigation/navigate-to-attack-docked-ship
;                          ship enemy-ship true)]
;                          ; ship enemy-ship (> (math/distance-between ship enemy-ship) 21.1)
;                          ; ship enemy-ship (> (math/distance-between ship enemy-ship) 12))]
;              :when move]
;          (do (map/change-ship-positions! move)
;              move))))))

(defn create-undock-moves
  "Create undock moves for the passed in ships."
  [ships]
  (log "Create undock moves for" ships)
  ;; Only undock if the ship shows up in the list more than once
  (let [grouped-ships (group-by :id ships)
        multiple-attacks (filter (fn [[k v]]
                                   ; (println "K is" k "V count is" (count v))
                                   (if (> *num-ships* 3)
                                     (>= (count v) (* 0.65 *num-ships*))
                                     (if (= 2 *num-players*)
                                       (>= (count v) 1)
                                       (>= (count v) 2))))
                                 grouped-ships)
        ships-to-undock (map (fn [[k v]] (first v)) multiple-attacks)
        planets-to-undock (set (map #(get-in % [:docking :planet]) ships-to-undock))
        planets-to-undock (filter #(not (map/good-surrounding-planet-helper (*planets* %) 55))
                                  planets-to-undock)]
    (flatten
     (for [planet-id planets-to-undock
           :let [planet (*planets* planet-id)
                 docking-ship-ids (get-in planet [:docking :ships])
                 docking-ships (map #(*ships* %) docking-ship-ids)]]
        (do
          (log "Docking ships" docking-ships "Planet" planet)
          (map e/undock-move (remove #(nil? (:id %)) docking-ships)))))))

    ; (log "Really undocking for" (mapv (fn [[k v]] (first v)) multiple-attacks))
    ; (map e/undock-move
    ;      (map (fn [[k v]]
    ;             (first v))
    ;           multiple-attacks))))

(comment
 ;; Test undock logic
  (def ships [#hlt.entity.Ship{:id 38, :pos #hlt.math.Position{:x 113.31427079601507, :y 96.8979402297031}, :health 128, :radius 0.5, :owner-id 0, :docking {:status :docked, :planet 1, :progress 0}} #hlt.entity.Ship{:id 82, :pos #hlt.math.Position{:x 110.41251442008456, :y 35.08759653764482}, :health 170, :radius 0.5, :owner-id 0, :docking {:status :docked, :planet 7, :progress 0}} #hlt.entity.Ship{:id 48, :pos #hlt.math.Position{:x 102.0717664832834, :y 71.29430026016966}, :health 255, :radius 0.5, :owner-id 0, :docking {:status :docked, :planet 2, :progress 0}} #hlt.entity.Ship{:id 48, :pos #hlt.math.Position{:x 102.0717664832834, :y 71.29430026016966}, :health 255, :radius 0.5, :owner-id 0, :docking {:status :docked, :planet 2, :progress 0}} #hlt.entity.Ship{:id 48, :pos #hlt.math.Position{:x 102.0717664832834, :y 71.29430026016966}, :health 255, :radius 0.5, :owner-id 0, :docking {:status :docked, :planet 2, :progress 0}} #hlt.entity.Ship{:id 53, :pos #hlt.math.Position{:x 105.44471871082905, :y 64.32571597974892}, :health 175, :radius 0.5, :owner-id 0, :docking {:status :docked, :planet 2, :progress 0}} #hlt.entity.Ship{:id 13, :pos #hlt.math.Position{:x 103.40284990201422, :y 88.87005159390104}, :health 255, :radius 0.5, :owner-id 0, :docking {:status :docked, :planet 1, :progress 0}} #hlt.entity.Ship{:id 38, :pos #hlt.math.Position{:x 113.31427079601507, :y 96.8979402297031}, :health 128, :radius 0.5, :owner-id 0, :docking {:status :docked, :planet 1, :progress 0}} #hlt.entity.Ship{:id 38, :pos #hlt.math.Position{:x 113.31427079601507, :y 96.8979402297031}, :health 128, :radius 0.5, :owner-id 0, :docking {:status :docked, :planet 1, :progress 0}} #hlt.entity.Ship{:id 38, :pos #hlt.math.Position{:x 113.31427079601507, :y 96.8979402297031}, :health 128, :radius 0.5, :owner-id 0, :docking {:status :docked, :planet 1, :progress 0}} #hlt.entity.Ship{:id 38, :pos #hlt.math.Position{:x 113.31427079601507, :y 96.8979402297031}, :health 128, :radius 0.5, :owner-id 0, :docking {:status :docked, :planet 1, :progress 0}} #hlt.entity.Ship{:id 82, :pos #hlt.math.Position{:x 110.41251442008456, :y 35.08759653764482}, :health 170, :radius 0.5, :owner-id 0, :docking {:status :docked, :planet 7, :progress 0}}])
 (group-by :id ships)
 (create-undock-moves ships))

(defn get-docked-ships-for-other-owners
  "Returns a map of player-id to their enemy docked ships (ones they want to take)."
  [docked-ships]
  (into {}
    (for [owner-id (keys *owner-ships*)]
      [owner-id (remove #(= owner-id (:owner-id %)) docked-ships)])))

(defn get-vulnerable-ships
  "Returns a list of vulnerable ships. This function does way too much - refactor this monstrosity."
  [my-ships]
  (let [all-docked-ships (remove #(= :undocked (-> % :docking :status))
                                 (vals *ships*))
        my-fighter-ships (filter #(= :undocked (-> % :docking :status))
                                my-ships)
        ; vulnerable-distance 75
        ; vulnerable-distance 49
        vulnerable-distance (if (> *num-ships* 5)
                              ; 42
                              55
                              62)
        docked-ships-other-by-owner (get-docked-ships-for-other-owners all-docked-ships)
        potential-issues (for [enemy-ship (vals *pesky-fighters*)
                               :let [owner-id (:owner-id enemy-ship)
                                     nearest-docked-ship (map/nearest-entity enemy-ship (docked-ships-other-by-owner owner-id))]
                               :when (= *player-id* (:owner-id nearest-docked-ship))
                               :let [distance (math/distance-between enemy-ship nearest-docked-ship)]
                               :when (< distance vulnerable-distance)]
                           {:enemy enemy-ship :vulnerable nearest-docked-ship
                            ; :distance vulnerable-distance
                            :distance distance})
        sorted-issues (sort (utils/compare-by :distance utils/asc) potential-issues)
        assigned-ships (atom nil)
        ships-to-undock (atom nil)
        vulnerable-ship-maps
          ;; Go through closest first
         (doall
          (for [{:keys [enemy vulnerable distance]} sorted-issues
                :let [closest-defender (map/nearest-entity vulnerable
                                                           (remove #(some (set [%]) @assigned-ships)
                                                                   my-fighter-ships))
                ; :when closest-defender
                      defender-distance (if-not closest-defender
                                          infinity
                                          (math/distance-between closest-defender vulnerable))]]
                ;; Close enough to defend
                ; :when (<= defender-distance (+ 14 distance))]
            (if (<= defender-distance (+ 14 distance))
            ; (if (<= defender-distance (+ 14 distance))
              (do
               (log "I can defend" vulnerable)
               (swap! assigned-ships conj closest-defender)
               [closest-defender vulnerable enemy])
              (do
               (log "I cannot defend" vulnerable)
               (swap! ships-to-undock conj vulnerable)
               nil))))
        vulnerable-ship-maps (remove nil? vulnerable-ship-maps)
        undock-moves (create-undock-moves @ships-to-undock)]
        ; undock-moves (create-undock-moves @ships-to-undock)]
    {:vulnerable-ships vulnerable-ship-maps
     :undock-moves undock-moves}))

(defn run-to-corner-moves
  "Run away"
  [my-ships]
  (let [my-undocked-ships (filter #(and (= *player-id* (:owner-id %))
                                        (= :undocked (-> % :docking :status)))
                                  my-ships)
        neutral-planet-count (count (filter #(nil? (:owner-id %))
                                            (vals *planets*)))]
    ; (when true)
    (when (and (> *num-players* 2)
               ;; Less than 10 percent of the total ships and no more than 4 neutral planets
               (<= neutral-planet-count 7)
               (< (count my-ships) (* 0.1 (count (vals *ships*)))))
      (let [moves (for [ship (take 10 my-undocked-ships)
                        :when ship]
                    (navigation/navigate-to-nearest-corner ship))]
        (for [move moves
              :when move]
          (do (map/change-ship-positions! move)
              move))))))

(defn defend-vulnerable-ships
  "Returns moves to defend vulnerable ships."
  [moving-ships {:keys [start-ms]}]
  (let [enemy-fighters (vals *pesky-fighters*)
        potential-ships (filter #(and (= *player-id* (:owner-id %))
                                      (not (some (set [(:id %)]) moving-ships)))
                                (vals *ships*))
        max-defenders (max 3 (* *num-ships* (/ 1 2)))
        ; max-defenders (* *num-ships* (/ 1 2))
        ; max-defenders *num-ships*
        ; max-defenders (* *num-ships* (/ 4 7))
        {:keys [undock-moves vulnerable-ships]} (get-vulnerable-ships potential-ships)
        vulnerable-ships (take max-defenders vulnerable-ships)]
    (concat undock-moves
            (doall
             (for [[defender ship enemy] vulnerable-ships
                   ; :let [advantage? (map/have-advantage? (custom-math/get-point-between ship enemy 0.8))
                   :let [times-up? (> (- (System/currentTimeMillis) *start-ms*) 1625)]
                   :when (not times-up?)
                   ; :let [advantage? (map/have-advantage? enemy)]
                   :let [
                         ; closest-enemy enemy
                         closest-enemy (map/nearest-entity ship enemy-fighters)
                         distance (math/distance-between ship closest-enemy)
                         ; advantage? false
                         advantage? (< distance 14)
                         ; advantage? (or (> distance 25)
                         ;                (map/have-advantage? (custom-math/get-closest-point-towards-target ship closest-enemy (- distance 3))))
                         move (get-reachable-attack-spot-move ship)
                         move (if move
                                move
                                (navigation/navigate-to-defend-ship defender ship closest-enemy advantage?))]
                   :when move]
               (do (map/change-ship-positions! move)
                   move))))))

(defn recalculate-friendly-moves
  "Returns the retreat to nearby friendly moves now that we know where all the other ships are
  going."
  [moves {:keys [start-ms]}]
  (let [ignore-ships (atom (mapv #(get-in % [:ship :id]) moves))
        enemy-ships (filter #(and (= :undocked (-> % :docking :status))
                                  (not= *player-id* (:owner-id %)))
                            (vals *ships*))]
  ; (let [ignore-ships (if (> *num-ships* 3)
  ;                      (atom (mapv #(get-in % [:ship :id]) moves))
  ;                      (atom nil))]
    (for [move moves
          :let [times-up? (> (- (System/currentTimeMillis) *start-ms*) 1625)]
          :when (not times-up?)
          :let [my-ships (map/get-my-real-ships)
                ship (:ship move)
                my-other-ships (remove #(or (= (:id ship) (:id %))
                                            (some (set [(:id %)]) @ignore-ships))
                                       my-ships)
                closest-friendly-ship (map/nearest-entity ship my-other-ships)]
          :let [new-move (if closest-friendly-ship
                           (navigation/navigate-to-friendly-ship ship closest-friendly-ship)
                           (let [closest-enemy-ship (map/nearest-entity ship enemy-ships)]
                             (navigation/navigate-to-retreat-ship ship closest-enemy-ship)))]
          :when new-move]
      (do
          (swap! ignore-ships #(remove (set [(:id ship)]) %))
          (map/change-ship-positions! (assoc new-move :subtype :friendly2))
          new-move))))

(def turns-to-produce-two-ships-with-three-docked
  (* 2 (/ 12 3)))

(def num-turns-to-kill-ship 2)

(defn go-for-corner-planet
  "Returns true or false if I should go for the corner planet."
  [turn]
  (and (< *num-ships* 4)
       (<= @all-out-attack 0)
       (>= turn 35)))
       ; (or (>= turn 50))))
           ; (> *num-players* 2))))

(defn get-best-planet
  "Returns the best planet to take. Corner planet in four player games or a 2 player stalemate."
  [turn]
  (if (go-for-corner-planet turn)
    (map/corner-big-planet)
    (when (> *num-ships* 35)
      (map/closest-planet-to-my-planets))))

(defn-timed starting-game-strategy!
  "Function called at beginning of game before starting."
  []
  ;; Initializes everything
  (map/get-custom-map-info 1)
  (let [four-center-planets (center-planet/find-four-center-planets)
        closet-planet-docking-spots-by-owner
        (into {}
              (for [owner-id (keys *owner-ships*)
                    :let [ship (first (vals (get *owner-ships* owner-id)))
                          planet (map/nearest-entity ship (vals *planets*))
                          pos (math/closest-point ship planet)
                          distance (math/distance-between ship pos)]]
                [owner-id {:pos pos :distance distance}]))
        enemy-positions (select-keys closet-planet-docking-spots-by-owner
                                     (remove #(= *player-id* %)
                                             (keys *owner-ships*)))
        my-ship (first (vals (get *owner-ships* *player-id*)))]
    (log "Avoiding" four-center-planets)
    ;; Num players wasn't calculated yet - this does nothing since it is always 0
    ; (when (> *num-players* 2)
    ;   (reset! map/avoid-planets (map :id four-center-planets)))
    (reset! center-planet/center-planets (map :id four-center-planets))
    (reset! map/best-planet (get-best-planet 1))
    (doseq [[owner {:keys [pos distance]}] enemy-positions]
      (let [distance-to-pos (math/distance-between my-ship pos)
            num-turns-to-planet (Math/ceil (/ (- distance e/dock-radius) e/max-ship-speed))
            num-turns-me-to-dock-spot (Math/ceil (/ distance-to-pos e/max-ship-speed))
            num-turns-to-two-ships (+ num-turns-to-planet e/dock-turns turns-to-produce-two-ships-with-three-docked)
            num-turns-to-attack (+ num-turns-me-to-dock-spot num-turns-to-kill-ship)]
        (when (<= num-turns-to-attack num-turns-to-two-ships))
          ; (reset! all-out-attack (inc num-turns-me-to-dock-spot))
          ;; Get rid of all out attack
          ; (reset! all-out-attack (+ 5 num-turns-to-two-ships)))
        (log "The distance between my ship" my-ship "and their planet " pos "is " distance-to-pos
             "total turns" num-turns-to-planet "and " num-turns-me-to-dock-spot
             "they produce 2 in " num-turns-to-two-ships "and I kill in" num-turns-to-attack)))))

(defn calculations-for-turn
  "Performs the calculations for the turn and returns the custom map info"
  [turn]
  (swap! all-out-attack dec)
  (map/get-custom-map-info turn))

(defn compute-move-to-enemy-ship
  "Picks the move for the ship based on proximity to planets and fighters near planets."
  [{:keys [start-ms moving-ships] :as custom-map-info} ship]
  (when (and (= :undocked (-> ship :docking :status))
             (not (some #{(:id ship)} moving-ships)))
    (when-let [move (move-to-nearest-enemy-ship ship (concat (vals *pesky-fighters*) (vals *docked-enemies*)))]
      (map/change-ship-positions! move)
      move)))

(defn-timed get-moves-to-enemy-ships
  "Returns the planet only moves"
  [ships-in-order custom-map-info]
  (doall
   (keep #(compute-move-to-enemy-ship custom-map-info %)
         ships-in-order)))

; (defn-timed get-main-moves
;   "Returns the main moves"
;   [ships-in-order custom-map-info]
;   (doall
;    (keep #(compute-move-closest-planet custom-map-info %)
;          ships-in-order)))

; (defn pick-moves-by-planets
;   "Returns moves by pulling ships to the planets."
;   [moving-ships]
;   (let [enemy-planets (map/sort-by-furthest (map/get-enemy-planets) (map/get-planets *player-id*))
;         my-ships (map/get-fighters *player-id* moving-ships)
;         assigned-ships (atom nil)]
;     (for [planet enemy-planets
;           :let [my-ships (remove #(some (set [%]) @assigned-ships)
;                                  my-ships)
;                 my-ship (map/nearest-entity planet my-ships)]
;           :when my-ship
;           :let [move (navigation/navigate-to-retreat my-ship planet)]
;           :when move]
;       (do
;         (swap! assigned-ships conj my-ship)
;         (map/change-ship-positions! move)
;         (assoc move :subtype :retreat5)))))
;
; (defn get-best-move
;   "Returns the best move for the current ship and target planet."
;   [start-ms ship target]
;   (let [times-up? (> (- (System/currentTimeMillis) *start-ms*) 1625)]
;     (if (or times-up?
;             (not= :undocked (-> ship :docking :status)))
;       nil
;       (let [move (get-reachable-attack-spot-move ship)]
;         (if move
;           move
;           (if (pos? @all-out-attack)
;             (move-to-nearest-enemy-ship ship (concat (vals *pesky-fighters*) (vals *docked-enemies*)))
;
;             (let [planets (filter #(e/any-remaining-docking-spots? %)
;                                   (vals *planets*))]
;               (let [nearest-planet
;                     (:nearest-planet
;                      (reduce (fn [{:keys [min-distance nearest-planet]} planet]
;                                (let [distance-to-planet (- (math/distance-between ship planet) (:radius planet))]
;                                  (if (< distance-to-planet min-distance)
;                                    {:min-distance distance-to-planet :nearest-planet planet}
;                                    {:min-distance min-distance :nearest-planet nearest-planet})))
;                              {:min-distance infinity}
;                              planets))]
;                 (if (and nearest-planet
;                          (some #{(:id nearest-planet)} (keys *safe-planets*))
;                          ; No fighters close to taking me
;                          (map/safe-to-dock? ship))
;                   (move-ship-to-planet! ship nearest-planet)
;                   (move-to-nearest-enemy-ship-or-target ship (concat (vals *pesky-fighters*) (vals *docked-enemies*))
;                                                         target))))))))))

; (defn sort-fighter-move-map
;   "Sorts the the fighter move map."
;   [fighter-move-map pois]
;   (let [maps-with-distance (for [{:keys [ship target]} fighter-move-map
;                                  :let [distance (map/distance-to-poi ship pois)]]
;                              {:ship ship :target target :distance distance})]
;     (sort (utils/compare-by :distance utils/asc) maps-with-distance)))

(defn planets-with-docking-spots
  "Return planets with docking spots open."
  []
  (filter e/any-remaining-docking-spots? (vals *planets*)))

; (defn get-fighter-moves
;   "Returns moves for the fighters. Initially just move towards the target planet ignoring ships."
;   [{:keys [start-ms]} fighter-move-map]
;   (let [pois (concat (vals *docked-enemies*) (vals *safe-planets*) (vals *pesky-fighters*))]
;     (for [{:keys [ship target]} (sort-fighter-move-map fighter-move-map pois)
;           :let [move (get-best-move *start-ms* ship target)]
;           :when move]
;       (do
;           ; (log "Move is" move)
;           (map/change-ship-positions! move)
;           move))))

(defn return-planet-move
  "Returns a move to move towards the nearest planet."
  [planet moving-ships]
  (let [potential-ships (filter #(and (= :undocked (-> % :docking :status))
                                      (= *player-id* (:owner-id %))
                                      (not (some (set [(:id %)]) moving-ships)))
                                (vals *ships*))
        docking-spot (when (and planet (center-planet/center-planet? planet))
                       (center-planet/get-best-docking-point nil planet))
        closest-ship (if docking-spot
                       (map/nearest-entity (assoc docking-spot :radius 0.0) potential-ships)
                       (map/nearest-entity planet potential-ships))]
    (when (and closest-ship
               (or (not (e/within-docking-range? closest-ship planet))
                   (map/safe-to-dock? closest-ship)))
      (when-let [move (move-ship-to-planet! closest-ship planet)]
        (do (map/change-ship-positions! move)
            move)))))

(defn get-best-planet-moves
  "Returns moves towards the best planet."
  [planet moving-ships]
  (when (and planet
             (map/good-surrounding-planet-helper planet 30))
             ; (some #{(:id planet)} (keys *safe-planets*)))
    (return-planet-move planet moving-ships)))

(defn get-moves-and-moving-ships
  "TODO Returns moves and moving ships - make it easier to reorder."
  [moves-fn existing-moves moving-ships]
  nil)

; (defn- process-swarm-moves
;   "Processes swarm moves."
;   [moves]
;   (for [move moves]
;     (do
;      (map/change-ship-positions! move)
;      move)))

(defn get-swarm-moves
  "Returns the swarm moves."
  [{:keys [start-ms]} ships]
  (let [retreat-range (get-retreat-range *num-ships*)
        ; enemy-ships (filter #(not= *player-id* (:owner-id %)) (vals *ships*))
        swarms (swarm/get-swarms ships)
        moves (doall
               (for [single-swarm swarms
                     :let [times-up? (> (- (System/currentTimeMillis) *start-ms*) 1625)]
                     :when (not times-up?)
                     :let [enemy-ships (concat (vals *docked-enemies*) (vals *pesky-fighters*))
                           swarm-moves (when (seq enemy-ships)
                                         (swarm/get-swarm-move single-swarm enemy-ships retreat-range
                                                               *player-id*))]
                     :when (seq swarm-moves)]
                 ; (do (process-swarm-moves swarm-moves)
                 swarm-moves))]
     (log "The swarm-moves " (pr-str moves))
     (flatten moves)))

(defn compute-planet-only-move*
  "Picks the move for the ship based on proximity to planets and fighters near planets."
  [{:keys [start-ms]} ship]
  (let [times-up? (> (- (System/currentTimeMillis) *start-ms*) 1625)]
    (if (or times-up?
            (not= :undocked (-> ship :docking :status)))
      nil
      (let [planets (filter #(or (nil? (:owner-id %))
                                 (and (= *player-id* (:owner-id %))
                                      (e/any-remaining-docking-spots? %)))
                             (vals *planets*))
            planet-turns (center-planet/get-turns-to-planet ship planets)
            fewest-turns (:turns (first planet-turns))
            closest-planet (:planet (first planet-turns))]
        ; (log "PS: planet-turns" planet-turns)
        (log "PS: fewest turns" fewest-turns)
        (log "PS: closest-planet" closest-planet)
        (when closest-planet
          (let [potential-planet-turns
                                      ; (if (<= fewest-turns 1)
                                         ; [(first planet-turns)]
                                         (filter #(<= (:turns %) (+ 6 fewest-turns))
                                                 planet-turns)
                ;; Limit to the three closest
                potential-planet-turns (take 3 (sort (utils/compare-by :turns utils/asc)
                                                     potential-planet-turns))
                potential-planets (map :planet potential-planet-turns)
                best-planet (first (sort (utils/compare-by :priority utils/asc)
                                         potential-planets))
                ;; if moving to the closest planet moves us towards the best planet, take it
                ;; otherwise go to the best planet
                ;; Don't forget to add the turns it takes to get to the best planet
                current-distance-to-best-planet (- (math/distance-between ship best-planet) (:radius best-planet))
                _ (log "PS: current-dist-to-best" current-distance-to-best-planet)
                dock-point (math/closest-point ship closest-planet 4)
                _ (log "PS: dock-point" dock-point)
                closest-planet-distance-to-best-planet (+ (* 7 (- fewest-turns 1.5))
                                                          (- (math/distance-between dock-point best-planet)
                                                             (:radius best-planet)))
                _ (log "PS: closest-planet-distance-to-best-planet" closest-planet-distance-to-best-planet)
                chosen-planet (if (<= closest-planet-distance-to-best-planet
                                      current-distance-to-best-planet)
                                closest-planet
                                best-planet)
                other-planet (if (<= closest-planet-distance-to-best-planet
                                     current-distance-to-best-planet)
                               best-planet
                               closest-planet)]
            (log "PS: potential-planets" potential-planets)
            (log "PS: chosen-planet" chosen-planet)
            (log "PS: other-planet" other-planet)
        ; (let [nearest-planet
        ;       (:nearest-planet
        ;        (reduce (fn [{:keys [min-distance nearest-planet]} planet]
        ;                  (let [distance-to-planet (- (math/distance-between ship planet) (:radius planet))]
        ;                    (if (< distance-to-planet min-distance)
        ;                      {:min-distance distance-to-planet :nearest-planet planet}
        ;                      {:min-distance min-distance :nearest-planet nearest-planet})))
        ;                {:min-distance infinity}
        ;                planets))]
            (when chosen-planet
              (if (and
                       (some #{(:id chosen-planet)} (keys *safe-planets*))
                       (or (not (e/within-docking-range? ship chosen-planet))
                           (and (map/good-surrounding-planet-helper-extra ship 35 0)
                                (map/good-surrounding-planet-helper-extra ship 25 0)
                                (map/good-surrounding-planet-helper-extra ship 15 0)
                                (map/safe-to-dock? ship))))
                (move-ship-to-planet! ship chosen-planet)))))))))
                ; (if (and
                ;          (some #{(:id other-planet)} (keys *safe-planets*))
                ;          (or (not (e/within-docking-range? ship other-planet))
                ;              (and (map/good-surrounding-planet-helper-extra ship 35 0)
                ;                   (map/good-surrounding-planet-helper-extra ship 25 0)
                ;                   (map/good-surrounding-planet-helper-extra ship 15 0)
                ;                   (map/safe-to-dock? ship))))
                ;   (move-ship-to-planet! ship other-planet))))))))))

; (defn compute-planet-only-move*
;   "Picks the move for the ship based on proximity to planets and fighters near planets."
;   [{:keys [start-ms]} ship]
;   (let [times-up? (> (- (System/currentTimeMillis) *start-ms*) 1625)]
;     (if (or times-up?
;             (not= :undocked (-> ship :docking :status)))
;       nil
;       (when-let [planets (filter #(or (nil? (:owner-id %))
;                                       (and (= *player-id* (:owner-id %))
;                                            (e/any-remaining-docking-spots? %)))
;                                  (vals *planets*))]
;         (let [nearest-planet
;               (:nearest-planet
;                (reduce (fn [{:keys [min-distance nearest-planet]} planet]
;                          (let [distance-to-planet (- (math/distance-between ship planet) (:radius planet))]
;                            (if (< distance-to-planet min-distance)
;                              {:min-distance distance-to-planet :nearest-planet planet}
;                              {:min-distance min-distance :nearest-planet nearest-planet})))
;                        {:min-distance infinity}
;                        planets))]
;           (when (and nearest-planet
;                      (some #{(:id nearest-planet)} (keys *safe-planets*))
;                      (map/safe-to-dock? ship))
;             (move-ship-to-planet! ship nearest-planet)))))))

(defn compute-planet-only-move
  "Picks the move for the ship based on proximity to planets and fighters near planets."
  [{:keys [start-ms moving-ships] :as custom-map-info} ship]
  (when-not (some #{(:id ship)} moving-ships)
    (when-let [move (compute-planet-only-move* custom-map-info ship)]
      (map/change-ship-positions! move)
      move)))

(defn-timed get-planet-only-moves
  "Returns the planet only moves"
  [ships custom-map-info]
  (let [ships-in-order (map/sort-ships-by-distance ships
                                                   (vals *safe-planets*))]
    (doall
     (keep #(compute-planet-only-move custom-map-info %)
           ships-in-order))))

; (defn get-best-planet
;   "Returns the best planet to take."
;   []
;   (if (= *num-players* 2)
;     (map/closest-dockable-planet)
;     (if (> *num-ships* 8)
;       (map/closest-dockable-planet)
;       (map/corner-planet))))

; (defn get-best-planet
;   "Returns the best planet to take. Corner planet in four player games or a 2 player stalemate."
;   [turn]
;   (when (> turn 50)
;     (if (go-for-corner-planet turn)
;       (if (>= *num-ships* 8)
;         (map/closest-planet-to-my-planets)
;         (map/corner-planet))
;         ; (map/corner-big-planet))
;       (map/closest-planet-to-my-planets))))

(defn being-rushed?
  "Returns true if I seem to be getting rushed."
  [turn]
  (let [num-planets (count (map/get-planets *player-id*))
        players-with-planets (map/players-with-planets)]
    (and (> *num-players* 2)
         (= 0 num-planets)
         (or (> turn 35)
             (and (> turn 15)
                  (>= players-with-planets 2))))))

(defn get-closest-safe-planet
  "Returns the closest safe planet to the ship."
  [ship]
  (map/nearest-entity ship (filter (fn [planet]
                                     (map/good-surrounding-planet-helper planet 100))
                                   (vals *safe-planets*))))

(defn compute-safest-planet-move*
  "Picks the move for the ship based on proximity to planets and fighters near planets."
  [ship]
  (let [times-up? (> (- (System/currentTimeMillis) *start-ms*) 1625)]
    (if (or times-up?
            (not= :undocked (-> ship :docking :status)))
      nil
      (when-let [planet (get-closest-safe-planet ship)]
        (move-ship-to-planet! ship planet)))))

(defn compute-safest-planet-move
  "Picks a move to get the ship near the closest safest planet."
  [moving-ships ship]
  (when-not (some #{(:id ship)} moving-ships)
    (when-let [move (compute-safest-planet-move* ship)]
      (map/change-ship-positions! move)
      move)))

(defn get-safest-planet-moves
  "Returns safe planet moves when I'm desperate to survive."
  [ships moving-ships]
  (doall
   (keep #(compute-safest-planet-move moving-ships %)
         ships)))

(defn get-rush-moves
  "Returns rush protection moves."
  [turn]
  (let [custom-map-info (calculations-for-turn turn)
        ships-in-order (map/sort-ships-by-distance
                        (vals (get *owner-ships* *player-id*))
                        (concat (vals *docked-enemies*) (vals *safe-planets*) (vals *pesky-fighters*)))
        runaway-moves (run-to-corner-moves (reverse ships-in-order))
        moving-ships (map #(get-in % [:ship :id]) runaway-moves)
        planet-moves (get-safest-planet-moves ships-in-order moving-ships)]
    (concat runaway-moves planet-moves)))

(defn get-moves-for-turn
  "Returns all of the moves for this turn."
  [turn]
  (if (being-rushed? turn)
    (get-rush-moves turn)
  ; (if true
  ; (if (= 10 turn) (throw (Exception. "quit")))
    (let [custom-map-info (calculations-for-turn turn)
          ships-in-order (map/sort-ships-by-distance
                          (vals (get *owner-ships* *player-id*))
                          (vals (deref map/top-player-docked-ships)))
          runaway-moves (run-to-corner-moves (reverse ships-in-order))
          moving-ships (map #(get-in % [:ship :id]) runaway-moves)

          ; best-planet (get-best-planet turn)
          ; best-planet-move (if (and best-planet (go-for-corner-planet turn))
          ;                    (get-best-planet-moves best-planet moving-ships)
          ;                    [])
          ; best-planet-moves (if (seq best-planet-move) (flatten [best-planet-move]) [])
          ; _ (log "Best planet is" best-planet "Best planet-moves are" best-planet-moves)
          ; moving-ships (map #(get-in % [:ship :id]) (concat runaway-moves best-planet-moves))

          ; skip-defense? true
          best-planet-moves []

          attack-moves []
          ; attack-moves (attack-unprotected-enemy-ships moving-ships custom-map-info)
          ; moving-ships (map #(get-in % [:ship :id]) (concat runaway-moves attack-moves best-planet-moves))

          pct-ships-to-skip-defense (if (> *num-players* 2)
                                      0.65
                                      0.6)
          skip-defense? (and (> *num-ships* 50)
                             (> *num-ships* (* pct-ships-to-skip-defense (count (vals *ships*)))))
          defend-moves (if skip-defense?
                         []
                         (defend-vulnerable-ships moving-ships custom-map-info))
          ; moving-ships (map #(get-in % [:ship :id]) (concat defend-moves runaway-moves best-planet-moves))
          moving-ships (map #(get-in % [:ship :id]) (concat defend-moves runaway-moves attack-moves best-planet-moves))

          attack-moves (attack-unprotected-enemy-ships moving-ships custom-map-info)
          moving-ships (map #(get-in % [:ship :id]) (concat runaway-moves attack-moves defend-moves best-planet-moves))

          attack-moves-old (attack-unprotected-enemy-ships-old moving-ships custom-map-info)
          moving-ships (map #(get-in % [:ship :id]) (concat attack-moves-old runaway-moves attack-moves defend-moves best-planet-moves))

          ; potential-ships (filter #(and (= :undocked (-> % :docking :status))
          ;                               (not (some (set [(:id %)]) moving-ships)))
          ;                         ships-in-order)
          ; swarm-moves (get-swarm-moves potential-ships)
          ; moving-ships (map #(get-in % [:ship :id]) runaway-moves swarm-moves defend-moves attack-moves)
          swarm-moves []

          best-planet (if (> turn 25)
                        (get-best-planet turn)
                        (deref map/best-planet))

          best-planet (when (nil? (:owner-id best-planet))
                        best-planet)

          best-planet-move (if (go-for-corner-planet turn)
                             (get-best-planet-moves best-planet moving-ships)
                             [])
          best-planet-moves (if (seq best-planet-move) (flatten [best-planet-move]) [])
          moving-ships (map #(get-in % [:ship :id]) (concat attack-moves-old runaway-moves attack-moves defend-moves best-planet-moves swarm-moves))

          more-planet-moves (get-planet-only-moves ships-in-order (assoc custom-map-info :moving-ships moving-ships))
          moving-ships (map #(get-in % [:ship :id]) (concat attack-moves-old runaway-moves attack-moves defend-moves more-planet-moves swarm-moves best-planet-moves))

          best-planet-move-2-players (if (and best-planet (not (go-for-corner-planet turn)))
                                       (get-best-planet-moves best-planet moving-ships)
                                       [])
          best-planet-moves-2-players (if (seq best-planet-move-2-players) (flatten [best-planet-move-2-players]) [])

          moving-ships (map #(get-in % [:ship :id]) (concat attack-moves-old runaway-moves attack-moves defend-moves more-planet-moves best-planet-moves swarm-moves best-planet-moves-2-players))
          ; potential-ships (filter #(and (= :undocked (-> % :docking :status))
          ;                               (not (some (set [(:id %)]) moving-ships)))
          ;                         ships-in-order)
          ; swarm-moves (if (> *num-ships* 3)
          ;               (get-swarm-moves custom-map-info potential-ships)
          ;               (get-swarm-moves custom-map-info potential-ships))
          ; moving-ships (map #(get-in % [:ship :id]) (concat runaway-moves swarm-moves defend-moves attack-moves best-planet-moves more-planet-moves))

          ;; TODO - can I just get rid of all this and let things naturally happen?
          ; my-fighters (map/get-fighters *player-id* moving-ships)
          ; assigned-fighter-to-targets (map/fighters-to-targets my-fighters)
          ; main-moves (get-fighter-moves custom-map-info assigned-fighter-to-targets)
          ; moving-ships (map #(get-in % [:ship :id]) (concat runaway-moves attack-moves defend-moves main-moves best-planet-moves swarm-moves more-planet-moves))
          ; ; _ (log "Moving ships: " moving-ships)
          potential-ships (filter #(and (= :undocked (-> % :docking :status))
                                        (not (some (set [(:id %)]) moving-ships)))
                                  ships-in-order)
          ; ; _ (log "potential ships: " potential-ships)
          swarm-moves (if (> *num-ships* 3)
                        (get-swarm-moves custom-map-info potential-ships)
                        (get-swarm-moves custom-map-info potential-ships))
                        ; [])
          moving-ships (map #(get-in % [:ship :id]) (concat attack-moves-old runaway-moves swarm-moves defend-moves attack-moves best-planet-moves more-planet-moves best-planet-moves-2-players))
          ; moving-ships (map #(get-in % [:ship :id]) (concat main-moves runaway-moves swarm-moves defend-moves attack-moves best-planet-moves more-planet-moves))
          fallback-moves (get-moves-to-enemy-ships ships-in-order (assoc custom-map-info :moving-ships moving-ships))
          ; all-moves (concat runaway-moves defend-moves attack-moves main-moves fallback-moves best-planet-moves swarm-moves more-planet-moves)
          all-moves (concat attack-moves-old runaway-moves defend-moves attack-moves fallback-moves best-planet-moves swarm-moves more-planet-moves best-planet-moves-2-players)
          friendly-moves (recalculate-friendly-moves (filter #(= :friendly (:subtype %)) all-moves) custom-map-info)

          ;; DEBUG
          moving-ships (map #(get-in % [:ship :id]) (concat all-moves friendly-moves))
          _ (log "Moving ships" moving-ships)
          _ (log "All moves" (pretty-log all-moves))
          _ (log "Friendly moves" (pretty-log friendly-moves))

          all-moves (remove #(= :friendly (:subtype %)) all-moves)
          all-moves (remove #(= 0 (:thrust %)) all-moves)]
          ; main-moves (remove #(= :friendly (:subtype %)) main-moves)
          ; main-moves (remove #(= 0 (:thrust %)) main-moves)
          ; fallback-moves (remove #(= :friendly (:subtype %)) fallback-moves)
          ; fallback-moves (remove #(= 0 (:thrust %)) fallback-moves)
          ; swarm-moves (remove #(= :friendly (:subtype %)) swarm-moves)
          ; swarm-moves (remove #(= 0 (:thrust %)) swarm-moves)]
      ; (if (= 64 turn)
      ;   (log "All ships" (pretty-log *ships*)))

      (concat all-moves friendly-moves))))
      ; all-moves))
      ; (concat runaway-moves defend-moves attack-moves main-moves fallback-moves friendly-moves best-planet-moves swarm-moves more-planet-moves)))


;; (import 'net.jafama.FastMath)
