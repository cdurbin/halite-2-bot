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
  ; (log "Trying to move to planet" planet "from ship" ship)
  (let [move (if (e/within-docking-range? ship planet)
               (do (set! *ships* (assoc-in *ships* [(:id ship) :docking :status] :custom-docking))
                   (e/dock-move ship planet))
               (navigation/navigate-to-dock ship planet))]
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
  (let [move (navigation/navigate-to-attack-ship ship enemy-ship)]
    (when (and move (pos? (:thrust move)))
      (let [fighter? (= :undocked (-> enemy-ship :docking :status))
            attack-count (inc (get enemy-ship :attack-count 0))
            remove? (= 5 (:attack-count enemy-ship))]
        (if fighter?
          (if remove?
            (set! *pesky-fighters* (dissoc *pesky-fighters* (:id enemy-ship)))
            (set! *pesky-fighters* (assoc-in *pesky-fighters* [(:id enemy-ship) :attack-count] attack-count)))
          (if remove?
            (set! *docked-enemies* (dissoc *docked-enemies* (:id enemy-ship)))
            (set! *docked-enemies* (assoc-in *docked-enemies* [(:id enemy-ship) :attack-count] attack-count))))))
    move))

(def retreat-if-this-close 25)

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
      (let [closest-enemy-planet (map/nearest-entity ship (map/get-planets (:owner-id enemy-ship)))]
        (if closest-enemy-planet
          (navigation/navigate-to-retreat ship closest-enemy-planet)
          (navigation/navigate-to-retreat-ship ship enemy-ship))))))

(defn move-ship-to-retreat-for-real
  "Moves the ship to retreat from the enemy ship."
  [ship enemy-ship]
  (let [closest-enemy-planet (map/nearest-entity ship (map/get-planets (:owner-id enemy-ship)))]
    (if closest-enemy-planet
      (navigation/navigate-to-retreat ship closest-enemy-planet)
      (navigation/navigate-to-retreat-ship ship enemy-ship))))

(def tag-team-range 5)
; (def retreat-range 21.1)
; (def retreat-range-early 35)

(def retreat-range 40)
(def retreat-range-early 60)

; (def retreat-range 50)
; (def retreat-range-early 70)

(def ignore-retreating-ship-count
  "Optimization to not worry about running away when I have this many ships."
  85)

(defn get-retreat-range
  "Returns the retreat range based on number of ships."
  [num-ships]
  (if (> num-ships 3)
    retreat-range
    retreat-range-early))

(defn move-to-nearest-enemy-ship-or-target
  "Moves the ship to the nearest enemy ship."
  [ship enemy-ships target]
  (when-let [enemy-ship (map/nearest-enemy-not-decoy ship enemy-ships)]
    (if (or (> *num-ships* ignore-retreating-ship-count)
            (> (math/distance-between ship enemy-ship) (get-retreat-range *num-ships*)))
      (move-ship-to-attack ship enemy-ship)
      (let [attack? (map/have-advantage? enemy-ship)]
        (if attack?
          (move-ship-to-attack ship enemy-ship)
          (if (map/alone? ship enemy-ship *player-id* tag-team-range false)
            (move-ship-to-retreat ship enemy-ship)
            (navigation/navigate-to-retreat ship target)))))))

(defn move-to-nearest-enemy-ship
  "Moves the ship to the nearest enemy ship."
  [ship enemy-ships]
  (when-let [enemy-ship (map/nearest-enemy-not-decoy ship enemy-ships)]
    (if (or (> *num-ships* ignore-retreating-ship-count)
            (> (math/distance-between ship enemy-ship) (get-retreat-range *num-ships*)))
      (move-ship-to-attack ship enemy-ship)
      (let [attack? (map/have-advantage? enemy-ship)]
        (if attack?
          (move-ship-to-attack ship enemy-ship)
          (if (map/alone? ship enemy-ship *player-id* tag-team-range false)
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
    (let [imaginary-ships (calculate-end-positions move)]
      (doseq [i-ship imaginary-ships]
        (set! *ships* (assoc *ships* (java.util.UUID/randomUUID) i-ship))))))

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
                             (if (< distance-to-planet min-distance)
                               {:min-distance distance-to-planet :nearest-planet planet}
                               {:min-distance min-distance :nearest-planet nearest-planet})))
                         {:min-distance infinity}
                         planets))]
            (if (and best-planet
                     (some #{(:id best-planet)} (keys *safe-planets*))
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

; (defn get-vulnerable-ships
;   "Returns a list of vulnerable ships."
;   [my-ships]
;   (let [my-docked-ships (remove #(= :undocked (-> % :docking :status))
;                                 my-ships)
;         my-fighter-ships (filter #(= :undocked (-> % :docking :status))
;                                 my-ships)
;         vulnerable-distance 42
;         ; vulnerable-distance 19
;         ; vulnerable-distance 30
;         potential-issues (for [enemy-ship (vals *pesky-fighters*)
;                                :let [nearest-docked-ship (map/nearest-entity enemy-ship my-docked-ships)]
;                                :when nearest-docked-ship
;                                :let [distance (math/distance-between enemy-ship nearest-docked-ship)]
;                                :when (< distance vulnerable-distance)]
;                            {:enemy enemy-ship :vulnerable nearest-docked-ship
;                             ; :distance vulnerable-distance
;                             :distance distance})
;         sorted-issues (sort (utils/compare-by :distance utils/asc) potential-issues)
;         assigned-ships (atom nil)]
;     ;; Go through closest first
;     ; (into {}
;     (for [{:keys [enemy vulnerable distance]} sorted-issues
;           :let [closest-defender (map/nearest-entity vulnerable
;                                                      (remove #(some (set [%]) @assigned-ships)
;                                                              my-fighter-ships))
;                 defender-distance (if closest-defender
;                                     (math/distance-between closest-defender vulnerable)
;                                     infinity)
;                 closest-defender (when (<= defender-distance (+ 14 distance)) closest-defender)]]
;           ;; Close enough to defend
;           ; :when (<= defender-distance (+ 14 distance))]
;       (do
;          (when closest-defender
;            (swap! assigned-ships conj closest-defender))
;          [closest-defender vulnerable enemy]))))

(defn get-vulnerable-ships
  "Returns a list of vulnerable ships."
  [my-ships]
  (let [my-docked-ships (remove #(= :undocked (-> % :docking :status))
                                my-ships)
        my-fighter-ships (filter #(= :undocked (-> % :docking :status))
                                my-ships)
        vulnerable-distance 42
        ; vulnerable-distance 28
        ; vulnerable-distance 19
        ; vulnerable-distance 30
        potential-issues (for [enemy-ship (vals *pesky-fighters*)
                               :let [nearest-docked-ship (map/nearest-entity enemy-ship my-docked-ships)]
                               :when nearest-docked-ship
                               :let [distance (math/distance-between enemy-ship nearest-docked-ship)]
                               :when (< distance vulnerable-distance)]
                           {:enemy enemy-ship :vulnerable nearest-docked-ship
                            ; :distance vulnerable-distance
                            :distance distance})
        sorted-issues (sort (utils/compare-by :distance utils/asc) potential-issues)
        assigned-ships (atom nil)]
    ;; Go through closest first
    ; (into {}
    (for [{:keys [enemy vulnerable distance]} sorted-issues
          :let [closest-defender (map/nearest-entity vulnerable
                                                     (remove #(some (set [%]) @assigned-ships)
                                                             my-fighter-ships))]
          :when closest-defender
          :let [defender-distance (math/distance-between closest-defender vulnerable)]
          ;; Close enough to defend
          :when (<= defender-distance (+ 14 distance))]
      (do
         (swap! assigned-ships conj closest-defender)
         [closest-defender vulnerable enemy]))))

(defn run-to-corner-moves
  "Run away"
  [my-ships]
  (let [my-undocked-ships (filter #(and (= *player-id* (:owner-id %))
                                        (= :undocked (-> % :docking :status)))
                                  my-ships)]
        ; num-undocked-ships (count my-ships)]
    (when (and (> *num-players* 2)
               ; (> (count (vals *ships*)) 60)
               ;; Less than 10 percent of the total ships
               (< (count my-ships) (* 0.1 (count (vals *ships*)))))
      (let [moves (for [ship (take 2 my-undocked-ships)
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
     (for [[defender ship enemy] vulnerable-ships
           :let [move (navigation/navigate-to-defend-ship defender ship enemy)]
           :when move]
       (do (change-ship-positions! move)
           move)))))

; (defn defend-vulnerable-ships
;   "Returns moves to defend vulnerable ships."
;   [moving-ships]
;   (let [potential-ships (filter #(and (= *player-id* (:owner-id %))
;                                       (not (some (set [(:id %)]) moving-ships)))
;                                 (vals *ships*))
;         vulnerable-ships (get-vulnerable-ships potential-ships)
;         undocking-ships (atom nil)]
;     (doall
;      (for [[defender ship enemy] vulnerable-ships
;            :let [move (if defender
;                         (navigation/navigate-to-defend-ship defender ship enemy)
;                         (if-not (some (set [(:id ship)]) @undocking-ships)
;                           (do (swap! undocking-ships conj (:id ship))
;                               (log "I am undocking" ship)
;                               (e/undock-move ship))))]
;
;            :when move]
;        (do (change-ship-positions! move)
;            move)))))

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
  (let [four-center-planets (map/find-four-center-planets)
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
    (reset! map/avoid-planets four-center-planets)
    (doseq [[owner {:keys [pos distance]}] enemy-positions]
      (let [distance-to-pos (math/distance-between my-ship pos)
            num-turns-to-planet (Math/ceil (/ (- distance e/dock-radius) e/max-ship-speed))
            num-turns-me-to-dock-spot (Math/ceil (/ distance-to-pos e/max-ship-speed))
            num-turns-to-two-ships (+ num-turns-to-planet e/dock-turns turns-to-produce-two-ships-with-three-docked)
            num-turns-to-attack (+ num-turns-me-to-dock-spot num-turns-to-kill-ship)]
        (when (<= num-turns-to-attack num-turns-to-two-ships)
          ; (reset! all-out-attack (inc num-turns-me-to-dock-spot))
          (reset! all-out-attack (+ 5 num-turns-to-two-ships)))
        (log "The distance between my ship" my-ship "and their planet " pos "is " distance-to-pos
             "total turns" num-turns-to-planet "and " num-turns-me-to-dock-spot
             "they produce 2 in " num-turns-to-two-ships "and I kill in" num-turns-to-attack)))))

(defn calculations-for-turn
  "Performs the calculations for the turn and returns the custom map info"
  [turn]
  (swap! all-out-attack dec)
  (map/get-custom-map-info turn))

; (defn-timed get-main-moves
;   "Returns the main moves"
;   [ships-in-order custom-map-info]
;   (doall
;    (keep #(compute-move-best-planet custom-map-info %)
;          ships-in-order)))

(defn-timed get-main-moves
  "Returns the main moves"
  [ships-in-order custom-map-info]
  (doall
   (keep #(compute-move-closest-planet custom-map-info %)
         ships-in-order)))

(defn pick-moves-by-planets
  "Returns moves by pulling ships to the planets."
  [moving-ships]
  (let [enemy-planets (map/sort-by-furthest (map/get-enemy-planets) (map/get-planets *player-id*))
        my-ships (map/get-fighters *player-id* moving-ships)
        assigned-ships (atom nil)]
    (for [planet enemy-planets
          :let [my-ships (remove #(some (set [%]) @assigned-ships)
                                 my-ships)
                my-ship (map/nearest-entity planet my-ships)]
          :when my-ship
          :let [move (navigation/navigate-to-retreat my-ship planet)]
          :when move]
      (do
        (swap! assigned-ships conj my-ship)
        (change-ship-positions! move)
        (assoc move :subtype :retreat5)))))

; (defn pick-moves-by-planets
;   "Returns moves by pulling ships to the planets. Changed to focus on the nearest enemy-planets"
;   [moving-ships]
;   (let [enemy-planets [(first (reverse (map/sort-by-furthest (map/get-enemy-planets) (map/get-planets *player-id*))))]
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
;         (change-ship-positions! move)
;         (assoc move :subtype :retreat5)))))

(defn get-best-move
  "Returns the best move for the current ship and target planet."
  [start-ms ship target]
  (let [times-up? (> (- (System/currentTimeMillis) start-ms) 1550)]
    (if (or times-up?
            (not= :undocked (-> ship :docking :status)))
      nil
      (if (pos? @all-out-attack)
        (move-to-nearest-enemy-ship ship (concat (vals *pesky-fighters*) (vals *docked-enemies*)))

        (let [planets (filter #(e/any-remaining-docking-spots? %)
                              (vals *planets*))]
        ; (when-let [planets (filter #(or (not= *player-id* (:owner-id %))
        ;                                 (e/any-remaining-docking-spots? %))
        ;                            (vals *planets*))]
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
                     ; No fighters close to taking me
                     (let [all-fighters (filter #(and (= :undocked (-> % :docking :status))
                                                      (not= (:id ship) (:id %)))
                                                (vals *ships*))
                           closest-fighter (map/nearest-entity ship all-fighters)]
                       (or (nil? closest-fighter) (= *player-id* (:owner-id closest-fighter)))))
              (move-ship-to-planet! ship nearest-planet)
              ; (move-to-nearest-enemy-ship-or-target ship (vals *docked-enemies*) target)
              (move-to-nearest-enemy-ship-or-target ship (concat (vals *pesky-fighters*) (vals *docked-enemies*))
                                                    target))))))))
              ; (navigation/navigate-to-retreat ship target))))))))

; (defn get-fighter-moves
;   "Returns moves for the fighters. Initially just move towards the target planet ignoring ships."
;   [{:keys [start-ms]} fighter-move-map]
;   ; (let [pois (concat (vals *docked-enemies*) (vals *safe-planets*) (vals *pesky-fighters*))
;   ;       ships-w-distance (map #(assoc % :distance (distance-to-poi % pois)) ships)]
;   ;   (mapv #(dissoc % :distance) (sort (utils/compare-by :distance utils/asc) ships-w-distance)))
;   (for [{:keys [ship target]} fighter-move-map
;         :let [
;               ; _ (log "Ship is " ship " and target is" target)
;               move (get-best-move start-ms ship target)]
;         :when move]
;     (do
;         ; (log "Move is" move)
;         (change-ship-positions! move)
;         move)))

(defn sort-fighter-move-map
  "Sorts the the fighter move map."
  [fighter-move-map pois]
  (let [maps-with-distance (for [{:keys [ship target]} fighter-move-map
                                 :let [distance (map/distance-to-poi ship pois)]]
                             {:ship ship :target target :distance distance})]
    (sort (utils/compare-by :distance utils/asc) maps-with-distance)))

(defn planets-with-docking-spots
  "Return planets with docking spots open."
  []
  (filter e/any-remaining-docking-spots? (vals *planets*)))

(defn get-fighter-moves
  "Returns moves for the fighters. Initially just move towards the target planet ignoring ships."
  [{:keys [start-ms]} fighter-move-map]
  (let [pois (concat (vals *docked-enemies*) (vals *safe-planets*) (vals *pesky-fighters*))]
  ; (let [pois (concat (vals *docked-enemies*) (planets-with-docking-spots) (vals *pesky-fighters*))]
    (for [{:keys [ship target]} (sort-fighter-move-map fighter-move-map pois)
          :let [move (get-best-move start-ms ship target)]
          :when move]
      (do
          ; (log "Move is" move)
          (change-ship-positions! move)
          move))))

(defn get-best-planet-moves
  "Returns moves towards the best planet."
  [planet moving-ships]
  (when (and
             (not= 2 *num-players*)
             (<= @all-out-attack 0)
             (<= *num-ships* 3)
             planet
             (some #{(:id planet)} (keys *safe-planets*)))

    (let [potential-ships (filter #(and (= :undocked (-> % :docking :status))
                                        (= *player-id* (:owner-id %))
                                        (not (some (set [(:id %)]) moving-ships)))
                                  (vals *ships*))
          ; potential-moves  (for [ship potential-ships
          ;                        :let [move (move-ship-to-planet! ship planet)]
          ;                        :when move]
          ;                    move)
          closest-ship (map/nearest-entity planet potential-ships)]
      (when (and closest-ship
                 (let [all-fighters (filter #(and (= :undocked (-> % :docking :status))
                                                  (not= (:id closest-ship) (:id %)))
                                            (vals *ships*))
                       closest-fighter (map/nearest-entity closest-ship all-fighters)]
                   (or (nil? closest-fighter) (= *player-id* (:owner-id closest-fighter)))))
        ; (for [move potential-moves]
        ;   (do (change-ship-positions! move)
        ;       move))
        (let [move (move-ship-to-planet! closest-ship planet)]
          (do (change-ship-positions! move)
              move))))))


(defn get-moves-for-turn
  "Returns all of the moves for this turn."
  [turn]
  (let [custom-map-info (calculations-for-turn turn)
        ships-in-order (map/sort-ships-by-distance (vals (get *owner-ships* *player-id*)))
        runaway-moves (run-to-corner-moves (reverse ships-in-order))
        moving-ships (map #(get-in % [:ship :id]) runaway-moves)
        ; best-planet (map/my-best-planet)
        ; best-planet-move (get-best-planet-moves best-planet moving-ships)
        ; ; best-planet-moves (get-best-planet-moves best-planet moving-ships)
        ; ; best-planet-moves (if (seq best-planet-moves) best-planet-moves [])
        ; best-planet-moves (if best-planet-move [best-planet-move] [])
        ; moving-ships (map #(get-in % [:ship :id]) (concat runaway-moves best-planet-moves))
        defend-moves (defend-vulnerable-ships moving-ships)
        ; moving-ships (map #(get-in % [:ship :id]) (concat defend-moves runaway-moves best-planet-moves))
        moving-ships (map #(get-in % [:ship :id]) (concat defend-moves runaway-moves))

        attack-moves (attack-unprotected-enemy-ships moving-ships)
        moving-ships (map #(get-in % [:ship :id]) (concat runaway-moves attack-moves defend-moves))
        ; moving-ships (map #(get-in % [:ship :id]) (concat runaway-moves attack-moves defend-moves best-planet-moves))
                       ; planet-moves (pick-moves-by-planets moving-ships)
                       ; moving-ships (map #(get-in % [:ship :id]) (concat runaway-moves attack-moves defend-moves planet-moves))

        best-planet (if (= *num-players* 2)
                      (map/my-best-planet)
                      (map/corner-planet))
                      ; (map/safest-planet))
        best-planet-move (get-best-planet-moves best-planet moving-ships)
        ; best-planet-moves (get-best-planet-moves best-planet moving-ships)
        ; best-planet-moves (if (seq best-planet-moves) best-planet-moves [])
        best-planet-moves (if (seq best-planet-move) (flatten [best-planet-move]) [])
        moving-ships (map #(get-in % [:ship :id]) (concat runaway-moves attack-moves defend-moves best-planet-moves))


        my-fighters (map/get-fighters *player-id* moving-ships)
        assigned-fighter-to-targets (map/fighters-to-targets my-fighters)
        main-moves (get-fighter-moves custom-map-info assigned-fighter-to-targets)
        moving-ships (map #(get-in % [:ship :id]) (concat runaway-moves attack-moves defend-moves main-moves best-planet-moves))
        fallback-moves (get-main-moves ships-in-order (assoc custom-map-info :moving-ships moving-ships))
        ; moving-ships (map #(get-in % [:ship :id]) (concat runaway-moves fighter-moves defend-moves))
        ; _ (log "Assigned fighters are" assigned-fighter-to-targets)
        ; friendly-moves (recalculate-friendly-moves (filter #(= :friendly (:subtype %)) fallback-moves))
        friendly-moves (recalculate-friendly-moves (filter #(= :friendly (:subtype %)) (concat main-moves fallback-moves)))
        main-moves (remove #(= :friendly (:subtype %)) main-moves)
        main-moves (remove #(= 0 (:thrust %)) main-moves)
        fallback-moves (remove #(= :friendly (:subtype %)) fallback-moves)
        fallback-moves (remove #(= 0 (:thrust %)) fallback-moves)]

    ; (log "Planet moves are:" planet-moves)
    ; (log "Runaway moves" runaway-moves)
    ; (log "=== Defend moves:" defend-moves)
    ; (log "=== Fighter moves:" fighter-moves)
    ; (log "Initial moves:" attack-moves)
    ; (log "Moves:" moves)
    ; (log "Friendly" friendly-moves)
    ; (io/send-moves (concat defend-moves attack-moves moves)))))
    ; (log "Moves are: " (concat runaway-moves defend-moves fighter-moves))
    ; (concat runaway-moves defend-moves fighter-moves)))
    ; (log "Moves are:" (concat runaway-moves defend-moves attack-moves main-moves fallback-moves friendly-moves))
    (concat runaway-moves defend-moves attack-moves main-moves fallback-moves friendly-moves best-planet-moves)))
    ; (concat runaway-moves defend-moves attack-moves fallback-moves friendly-moves)))
    ; (concat runaway-moves defend-moves attack-moves main-moves friendly-moves planet-moves)))

; (defn get-moves-for-turn
;   "Returns all of the moves for this turn."
;   [turn]
;   (let [custom-map-info (calculations-for-turn turn)
;         ships-in-order (map/sort-ships-by-distance (vals (get *owner-ships* *player-id*)))
;         runaway-moves (run-to-corner-moves (reverse ships-in-order))
;         moving-ships (map #(get-in % [:ship :id]) runaway-moves)
;         defend-moves (defend-vulnerable-ships moving-ships)
;         moving-ships (map #(get-in % [:ship :id]) (concat defend-moves runaway-moves))
;         attack-moves (attack-unprotected-enemy-ships moving-ships)
;         moving-ships (map #(get-in % [:ship :id]) (concat runaway-moves attack-moves defend-moves))
;         ; planet-moves (pick-moves-by-planets moving-ships)
;         ; moving-ships (map #(get-in % [:ship :id]) (concat runaway-moves attack-moves defend-moves planet-moves))
;         main-moves (get-main-moves ships-in-order (assoc custom-map-info :moving-ships moving-ships))
;         friendly-moves (filter #(= :friendly (:subtype %)) main-moves)
;         main-moves (remove #(= :friendly (:subtype %)) main-moves)
;         main-moves (remove #(= 0 (:thrust %)) main-moves)]
;
;     ; (log "Planet moves are:" planet-moves)
;     ; (log "Runaway moves" runaway-moves)
;     ; (log "=== Defend moves:" defend-moves)
;     ; (log "Initial moves:" attack-moves)
;     ; (log "Moves:" moves)
;     ; (log "Friendly" friendly-moves)
;     ; (io/send-moves (concat defend-moves attack-moves moves)))))
;     (concat runaway-moves defend-moves attack-moves main-moves friendly-moves)))
;     ; (concat runaway-moves defend-moves attack-moves main-moves friendly-moves planet-moves)))

(comment
 (let [stuff [{:a 75 :b 15} {:a 75 :b 32} {:a 12 :b 32} {:a 9 :b 15}]]
    (vals (group-by :a stuff))))
