(ns custom.durbinator
  (:require
   [custom.game-map :refer [*safe-planets* *docked-enemies* *pesky-fighters* *num-ships*
                            *num-players*]]
   [custom.math :as custom-math :refer [infinity]]
   [custom.navigation :as navigation]
   [custom.utils :as utils :refer [defn-timed]]
   [custom.map-analysis :as map]
   [hlt.entity :as e]
   [hlt.game-map :refer [*player-id* *map-size* *bot-name* *ships* *planets* *owner-ships*]]
   [hlt.math :as math]
   [hlt.utils :refer [log]])
  (:import (java.io PrintWriter)))

(def my-bot-name "Durbinator")

(def all-out-attack
  (atom 0))

(defn move-ship-to-planet!
  "Moves the ship to the given planet. Side effect to update the planet to reduce the number of
  available docking spots by one."
  [ship planet]
  (let [upd-planet (update-in planet [:docking :ships] conj ship)]
    (set! *planets* (assoc *planets* (:id planet) upd-planet))
    (if-not (e/any-remaining-docking-spots? upd-planet)
      (set! *safe-planets* (dissoc *safe-planets* (:id planet)))
      (set! *safe-planets* (assoc *safe-planets* (:id planet) upd-planet))))
  (if (e/within-docking-range? ship planet)
    (do
      (set! *ships* (assoc-in *ships* [(:id ship) :docking :status] :custom-docking))
      (e/dock-move ship planet))
    (navigation/navigate-to-dock ship planet)))



; (defn move-ship-to-attack
;   "Moves the ship to attack the enemy ship."
;   [ship enemy-ship]
;   (if (= :undocked (-> enemy-ship :docking :status))
;     (navigation/navigate-to-attack-ship ship enemy-ship)
;     (navigation/navigate-to-attack-docked-ship ship enemy-ship)))

; (defn move-ship-to-attack
;   "Moves the ship to attack the enemy ship."
;   [ship enemy-ship]
;   ; (log "Pesky" *pesky-fighters*)
;   (let [fighter? (= :undocked (-> enemy-ship :docking :status))
;         attack-count (inc (get enemy-ship :attack-count 0))
;         ;_ (log "Atack count: " attack-count "enemy-ship" enemy-ship)
;         remove? (= 2 (:attack-count enemy-ship))]
;     (if fighter?
;       (if remove?
;         (do ;(log "Removing fighting enemy-ship" enemy-ship)
;             (set! *pesky-fighters* (dissoc *pesky-fighters* (:id enemy-ship))))
;         (do ;(log "Setting attack-count for " enemy-ship)
;             ;log "Before: " (get *pesky-fighters* (:id enemy-ship)))
;             (set! *pesky-fighters* (assoc-in *pesky-fighters* [(:id enemy-ship) :attack-count] attack-count))))
;             ; (log "After: " (get *pesky-fighters* (:id enemy-ship)))))
;       (if remove?
;         (do ;(log "Removing docked enemy-ship" enemy-ship)
;             (set! *docked-enemies* (dissoc *docked-enemies* (:id enemy-ship))))
;         (set! *docked-enemies* (assoc-in *docked-enemies* [(:id enemy-ship) :attack-count] attack-count)))))
;   (navigation/navigate-to-attack-ship ship enemy-ship))


(defn move-ship-to-attack
  "Moves the ship to attack the enemy ship."
  [ship enemy-ship]
  ; (log "Pesky" *pesky-fighters*)
  (let [move (navigation/navigate-to-attack-ship ship enemy-ship)]
    (when (and move (pos? (:thrust move)))
      (let [fighter? (= :undocked (-> enemy-ship :docking :status))
            attack-count (inc (get enemy-ship :attack-count 0))
            ;_ (log "Atack count: " attack-count "enemy-ship" enemy-ship)
            remove? (= 7 (:attack-count enemy-ship))]
        (if fighter?
          (if remove?
            (do ;(log "Removing fighting enemy-ship" enemy-ship)
                (set! *pesky-fighters* (dissoc *pesky-fighters* (:id enemy-ship))))
            (do ;(log "Setting attack-count for " enemy-ship)
                ;log "Before: " (get *pesky-fighters* (:id enemy-ship)))
                (set! *pesky-fighters* (assoc-in *pesky-fighters* [(:id enemy-ship) :attack-count] attack-count))))
                ; (log "After: " (get *pesky-fighters* (:id enemy-ship)))))
          (if remove?
            (do ;(log "Removing docked enemy-ship" enemy-ship)
                (set! *docked-enemies* (dissoc *docked-enemies* (:id enemy-ship))))
            (set! *docked-enemies* (assoc-in *docked-enemies* [(:id enemy-ship) :attack-count] attack-count))))))
    move))

(def retreat-if-this-close 10)

(defn move-ship-to-retreat
  "Moves the ship to retreat from the enemy ship."
  [ship enemy-ship]
  (let [my-ships (map/get-my-ships) ;; TODO should this only be fighters?
        my-other-ships (remove #(= (:id ship) (:id %))
                               my-ships)
        closest-friendly-ship (map/nearest-entity ship my-other-ships)]
    (if (and closest-friendly-ship
             (< (math/distance-between closest-friendly-ship ship) retreat-if-this-close))
      (navigation/navigate-to-friendly-ship ship closest-friendly-ship)
      (let [closest-enemy-planet (map/nearest-entity ship (filter #(= (:owner-id enemy-ship)
                                                                      (:owner-id %))
                                                                  (vals *planets*)))]
        (if closest-enemy-planet
          (navigation/navigate-to-retreat ship closest-enemy-planet)
          (navigation/navigate-to-retreat-ship ship enemy-ship))))))

(defn move-ship-to-retreat-for-real
  "Moves the ship to retreat from the enemy ship."
  [ship enemy-ship]
  (let [
        ; my-closest-planet (nearest-enemy-planet ship (filter #(= *player-id* (:owner-id %))
        ;                                                      (vals *planets*)))
        closest-enemy-planet (map/nearest-entity ship (filter #(= (:owner-id enemy-ship)
                                                                  (:owner-id %))
                                                              (vals *planets*)))]
    ; (if my-closest-planet
    (if closest-enemy-planet
      (navigation/navigate-to-retreat ship closest-enemy-planet)
      ; (navigation/navigate-to-retreat ship my-closest-planet)
      (navigation/navigate-to-retreat-ship ship enemy-ship))))

(def tag-team-range 5)
; (def tag-team-range (+ e/max-ship-speed e/ship-radius e/weapon-radius))
(def retreat-range 21.1)
(def retreat-range-early 35)
; (def retreat-range (- 21.1 7))

(def ignore-retreating-ship-count
  "Optimization to not worry about running away when I have this many ships."
  85)

(defn move-to-nearest-enemy-ship
  "Moves the ship to the nearest enemy ship."
  [ship enemy-ships]
  (when-let [enemy-ship (map/nearest-enemy-not-decoy ship enemy-ships)]
    ; (let [enemy-fighters (filter #(and (not= *player-id* (:owner-id %))
    ;                                    (= :undocked (-> % :docking :status)))
    ;                              (vals *ships*))]
    (if (or (> *num-ships* ignore-retreating-ship-count)
            (> (math/distance-between ship enemy-ship) (if (> *num-ships* 3)
                                                         retreat-range
                                                         retreat-range-early)))
      (move-ship-to-attack ship enemy-ship)
      (let [attack? (map/have-advantage? enemy-ship)]
            ; _ (log "Have advantage" attack? "for" ship)]
            ; (not (alone? ship enemy-ship *player-id* tag-team-range false))
            ; (have-advantage? ship)]
            ; (navigation/unreachable? enemy-ship enemy-ships))
        (if attack?
          (move-ship-to-attack ship enemy-ship)
          (if (map/alone? ship enemy-ship *player-id* tag-team-range false)
          ; (if (alone? ship enemy-ship *player-id* tag-team-range true)
            (move-ship-to-retreat ship enemy-ship)
            (move-ship-to-retreat-for-real ship enemy-ship)))))))

(defn compute-move-closest-planet*
  "Picks the move for the ship based on proximity to planets and fighters near planets."
  [{:keys [start-ms]} ship]
  (let [times-up? (> (- (System/currentTimeMillis) start-ms) 1550)]
    (if (or times-up?
            (not= :undocked (-> ship :docking :status)))
      nil
      (if (pos? @all-out-attack)
        (move-to-nearest-enemy-ship ship (concat (vals *pesky-fighters*) (vals *docked-enemies*)))

        (when-let [planets (filter #(or (not= *player-id* (:owner-id %))
                                        (e/any-remaining-docking-spots? %))
                                   (vals *planets*))]
          (let [nearest-planet
                (:nearest-planet
                 (reduce (fn [{:keys [min-distance nearest-planet]} planet]
                           (let [distance-to-planet (- (math/distance-between ship planet) (:radius planet))]
                             (if (< distance-to-planet min-distance)
                               {:min-distance distance-to-planet :nearest-planet planet}
                               {:min-distance min-distance :nearest-planet nearest-planet})))
                         {:min-distance infinity}
                         planets))]
            (if (and nearest-planet
                     (some #{(:id nearest-planet)} (keys *safe-planets*))
                         ; (nil? (first (concat (vals *pesky-fighters*) (vals *docked-enemies*))))))

                     ; No fighters close to taking me
                     (let [all-fighters (filter #(and (= :undocked (-> ship :docking :status))
                                                      (not= (:id ship) (:id %)))
                                                (vals *ships*))
                           closest-fighter (map/nearest-entity ship all-fighters)]
                       (or (nil? closest-fighter) (= *player-id* (:owner-id closest-fighter)))))
              (move-ship-to-planet! ship nearest-planet)
              (move-to-nearest-enemy-ship ship (concat (vals *pesky-fighters*) (vals *docked-enemies*))))))))))

(defn calculate-end-positions
  "Returns a ship in its end position based on thrust for this turn."
  [{:keys [ship thrust angle] :as move}]
  (let [x (get-in ship [:pos :x])
        y (get-in ship [:pos :y])
        positions (map math/map->Position
                       (custom-math/all-positions-start-to-end x y thrust angle))]
    (map #(assoc ship :pos %) positions)))

(defn change-ship-positions!
  "Changes a ships position in the main ships."
  [{:keys [ship type subtype] :as move}]
  (when (and (= :thrust type))
            ; (not= :friendly subtype))
    (let [imaginary-ships (calculate-end-positions move)]
      (doseq [i-ship imaginary-ships]
        (set! *ships* (assoc *ships* (java.util.UUID/randomUUID) i-ship))))))

; (defn change-ship-positions!
;   "Changes a ships position in the main ships."
;   [{:keys [ship type subtype] :as move}]
;   (when (and (= :thrust type) (not= :friendly subtype))
;     (when-let [imaginary-ships (seq (calculate-end-positions move))]
;       (doseq [i-ship (butlast imaginary-ships)]
;         (set! *ships* (assoc *ships* (java.util.UUID/randomUUID) i-ship)))
;       (set! *ships* (assoc *ships* (:id ship) (last imaginary-ships)
;                                    (java.util.UUID/randomUUID) ship)))))

(defn compute-move-closest-planet
  "Picks the move for the ship based on proximity to planets and fighters near planets."
  [{:keys [start-ms moving-ships] :as custom-map-info} ship]
  (when-not (some #{(:id ship)} moving-ships)
    (when-let [move (compute-move-closest-planet* custom-map-info ship)]
      (change-ship-positions! move)
      move)))

(defn compute-move-best-planet*
  "Picks the move for the ship based on proximity to planets and fighters near planets."
  [{:keys [start-ms]} ship]
  (let [times-up? (> (- (System/currentTimeMillis) start-ms) 1550)]
    (if (or times-up?
            (not= :undocked (-> ship :docking :status)))
      nil
      (if (pos? @all-out-attack)
        (move-to-nearest-enemy-ship ship (concat (vals *pesky-fighters*) (vals *docked-enemies*)))

        (when-let [planets (filter #(or (not= *player-id* (:owner-id %))
                                        (e/any-remaining-docking-spots? %))
                                   (vals *planets*))]
          (let [best-planet
                (:nearest-planet
                 (reduce (fn [{:keys [min-distance nearest-planet]} planet]
                           (let [distance-to-planet (- (math/square (- (math/distance-between ship planet)
                                                                       (:radius planet)))
                                                       (get planet :square-value 0))]
                                                       ; (/ (get planet :square-value 0) 100))]
                             (if (< distance-to-planet min-distance)
                               {:min-distance distance-to-planet :nearest-planet planet}
                               {:min-distance min-distance :nearest-planet nearest-planet})))
                         {:min-distance infinity}
                         planets))]
            (if (and best-planet
                     (some #{(:id best-planet)} (keys *safe-planets*))
                         ; (nil? (first (concat (vals *pesky-fighters*) (vals *docked-enemies*))))))

                     ; No fighters close to taking me
                     (let [all-fighters (filter #(and (= :undocked (-> ship :docking :status))
                                                      (not= (:id ship) (:id %)))
                                                (vals *ships*))
                           closest-fighter (map/nearest-entity ship all-fighters)]
                       (or (nil? closest-fighter) (= *player-id* (:owner-id closest-fighter)))))
              (move-ship-to-planet! ship best-planet)
              (move-to-nearest-enemy-ship ship (concat (vals *pesky-fighters*) (vals *docked-enemies*))))))))))

(defn compute-move-best-planet
  "Picks the move for the ship based on proximity to planets and fighters near planets."
  [{:keys [start-ms moving-ships] :as custom-map-info} ship]
  (when-not (some #{(:id ship)} moving-ships)
    (when-let [move (compute-move-best-planet* custom-map-info ship)]
      (change-ship-positions! move)
      move)))

(defn nearest-planet-distance
  "Returns the closest distance from one planet to any planet in a list."
  [orig-planet planets]
  (reduce (fn [min-distance planet]
             (let [distance-to-planet (- (math/distance-between orig-planet planet)
                                         (:radius planet)
                                         (:radius orig-planet))]
               (if (< distance-to-planet min-distance)
                 distance-to-planet
                 min-distance)))
          infinity
          planets))

(def unprotected-distance
  "How far away we look for unprotected ships."
  55)

(defn unprotected-enemy-ships
  "Returns a map of unprotected enemy ships with my ship as the key and the enemy ship as the value.
  TODO - this will only find a single dock point most of the time because the same ship of mine
  is probably the closest to several spots. Remove my ship each time it is selected. Will require
  several passes."
  [moving-ships]
  (let [distance unprotected-distance
        ; distance (if (or (< *num-ships* 5) (= *num-players* 2))
        ;            unprotected-distance
        ;            15)

        potential-ships (filter #(and (= :undocked (-> % :docking :status))
                                      (not (some (set [(:id %)]) moving-ships)))
                                (vals *ships*))
        assigned-ships (atom nil)
        all-permutations
         (for [enemy-ship (vals *docked-enemies*)
               :let [no-help (navigation/unreachable? enemy-ship (filter #(= (:owner-id enemy-ship)
                                                                             (:owner-id %))
                                                                         potential-ships))]
               :when no-help
               :let [closest-ship (map/nearest-entity enemy-ship
                                                      (remove #(or (= (:id enemy-ship) (:id %))
                                                                   (some (set [%]) @assigned-ships))
                                                              potential-ships))]
               :when (and (= *player-id* (:owner-id closest-ship))
                          (not (some #{closest-ship} @assigned-ships)))
               :let [distance (math/distance-between enemy-ship closest-ship)]
               :when (< distance unprotected-distance)]
           (do
             (swap! assigned-ships conj closest-ship)
             {:ship closest-ship
              :enemy-ship enemy-ship
              :distance distance}))
        sorted-order (sort (utils/compare-by :distance utils/desc) all-permutations)]
    ;; This makes sure that if the same ship is the closest to multiple it picks the closest one.
    (into {}
      (for [triple-map sorted-order]
        [(:ship triple-map) (:enemy-ship triple-map)]))))

(defn attack-unprotected-enemy-ships
  "Returns moves to attack the unprotected enemy ships"
  [moving-ships]
  (let [ship-attacks (unprotected-enemy-ships moving-ships)]
    (log "unprotected enemy ships are:" ship-attacks)
    (doall
     (for [[ship enemy-ship] ship-attacks
           :let [move (navigation/navigate-to-attack-ship ship enemy-ship)]
           :when move]
       (do (change-ship-positions! move)
           move)))))

(defn get-vulnerable-ships
  "Returns a list of vulnerable ships."
  [my-ships]
  (let [my-docked-ships (remove #(= :undocked (-> % :docking :status))
                                my-ships)
        my-fighter-ships (filter #(= :undocked (-> % :docking :status))
                                my-ships)
        vulnerable-distance 27
        potential-issues (for [enemy-ship (vals *pesky-fighters*)
                               :let [nearest-docked-ship (map/nearest-entity enemy-ship my-docked-ships)]
                               :when nearest-docked-ship
                               :let [distance (math/distance-between enemy-ship nearest-docked-ship)]
                               :when (< distance vulnerable-distance)]
                           {:enemy enemy-ship :vulnerable nearest-docked-ship
                            :distance vulnerable-distance})
        sorted-issues (sort (utils/compare-by :distance utils/asc) potential-issues)
        assigned-ships (atom nil)]
    ;; Go through closest first
    (into {}
      (for [{:keys [enemy vulnerable distance]} sorted-issues
            :let [closest-defender (map/nearest-entity vulnerable
                                                       (remove #(some (set [%]) @assigned-ships)
                                                               my-fighter-ships))]
            :when closest-defender
            :let [defender-distance (math/distance-between closest-defender vulnerable)]
            ;; Close enough to defend
            :when (<= defender-distance (+ 7 distance))]
        (do
           (swap! assigned-ships conj closest-defender)
           [closest-defender vulnerable])))))
           ; [closest-defender enemy])))))

(defn run-to-corner-moves
  "Run away"
  [my-ships]
  (let [my-ships (filter #(and (= *player-id* (:owner-id %))
                               (= :undocked (-> % :docking :status)))
                         my-ships)
        num-undocked-ships (count my-ships)]
    (when (and (> *num-players* 2)
               (> (count (vals *ships*)) 60)
               (< num-undocked-ships 6))
      (let [moves (for [ship (take 2 my-ships)
                        :when ship]
                    (navigation/navigate-to-nearest-corner ship))]
        (for [move moves
              :when move]
          (do (change-ship-positions! move)
              move))))))

(defn defend-vulnerable-ships
  "Returns moves to defend vulnerable ships."
  [moving-ships]
  (let [potential-ships (filter #(and (= *player-id* (:owner-id %))
                                      (not (some (set [(:id %)]) moving-ships)))
                                (vals *ships*))
        vulnerable-ships (get-vulnerable-ships potential-ships)]
    (doall
     (for [[defender ship] vulnerable-ships
           :let [move (navigation/navigate-to-friendly-ship defender ship)]
           :when move]
       (do (change-ship-positions! move)
           move)))))

(defn recalculate-friendly-moves
  "Returns the retreat to nearby friendly moves now that we know where all the other ships are
  going."
  [moves]
  (for [move moves
        :let [my-ships (map/get-my-ships)
              ship (:ship move)
              my-other-ships (remove #(= (:id ship) (:id %))
                                     my-ships)
              closest-friendly-ship (map/nearest-entity ship my-other-ships)
              new-move (navigation/navigate-to-friendly-ship ship closest-friendly-ship)]
        :when new-move]
    (do (change-ship-positions! (assoc new-move :subtype :friendly2))
        new-move)))

(def turns-to-produce-two-ships-with-three-docked
  (* 2 (/ 12 3)))

(def num-turns-to-kill-ship 2)

(defn-timed starting-game-strategy!
  "Function called at beginning of game before starting."
  []
  (let [closet-planet-docking-spots-by-owner
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
    (doseq [[owner {:keys [pos distance]}] enemy-positions]
      (let [distance-to-pos (math/distance-between my-ship pos)
            num-turns-to-planet (Math/ceil (/ (- distance e/dock-radius) e/max-ship-speed))
            num-turns-me-to-dock-spot (Math/ceil (/ distance-to-pos e/max-ship-speed))
            num-turns-to-two-ships (+ num-turns-to-planet e/dock-turns turns-to-produce-two-ships-with-three-docked)
            num-turns-to-attack (+ num-turns-me-to-dock-spot num-turns-to-kill-ship)]
        (when (<= num-turns-to-attack num-turns-to-two-ships)
          (reset! all-out-attack (inc num-turns-me-to-dock-spot)))
        (log "The distance between my ship" my-ship "and their planet " pos "is " distance-to-pos
             "total turns" num-turns-to-planet "and " num-turns-me-to-dock-spot
             "they produce 2 in " num-turns-to-two-ships "and I kill in" num-turns-to-attack)))))

(defn calculations-for-turn
  "Performs the calculations for the turn and returns the custom map info"
  [turn]
  (swap! all-out-attack dec)
  (map/get-custom-map-info turn))


(comment
 (let [stuff [{:a 75 :b 15} {:a 75 :b 32} {:a 12 :b 32} {:a 9 :b 15}]]
    (vals (group-by :a stuff))))
