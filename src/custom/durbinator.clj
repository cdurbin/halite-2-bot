(ns custom.durbinator
  (:require
   [clojure.stacktrace :as stack]
   [custom.game-map :refer [*safe-planets* *docked-enemies* *pesky-fighters* *num-ships*
                            *num-players*]]
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

(def close-to-dock-point 1)

(defn move-ship-to-planet!
  "Moves the ship to the given planet. Side effect to update the planet to reduce the number of
  available docking spots by one."
  [ship planet]
  ; (log "Trying to move to planet" planet "from ship" ship)
  (let [docking-point (center-planet/get-best-docking-point ship planet)
        move (if (and (e/within-docking-range? ship planet)
                      (< (math/distance-between ship docking-point) close-to-dock-point))
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
  (let [move (navigation/navigate-to-attack-ship ship enemy-ship
                                                 ; false)]
                                                 (> (math/distance-between ship enemy-ship) 21.1))]
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
  ; (let [my-ships (map/get-my-real-ships) ;; TODO should this only be fighters?
  (let [my-ships (map/get-fighters *player-id*) ;; TODO should this only be fighters?
        my-other-ships (remove #(= (:id ship) (:id %))
                               my-ships)
        closest-friendly-ship (map/nearest-entity ship my-other-ships)]
    (if (and closest-friendly-ship
             (< (math/distance-between closest-friendly-ship ship) retreat-if-this-close))
      (navigation/navigate-to-friendly-ship-later ship closest-friendly-ship)
      (let [closest-enemy-planet (map/farthest-entity ship (map/get-planets (:owner-id enemy-ship)))]
        (if closest-enemy-planet
          (navigation/navigate-to-retreat ship closest-enemy-planet)
          (navigation/navigate-to-retreat-ship ship enemy-ship))))))

(defn move-ship-to-retreat-for-real
  "Moves the ship to retreat from the enemy ship."
  [ship enemy-ship]
  (let [closest-enemy-planet (map/farthest-entity ship (map/get-planets (:owner-id enemy-ship)))]
    (if closest-enemy-planet
      (navigation/navigate-to-retreat ship closest-enemy-planet)
      (navigation/navigate-to-retreat-ship ship enemy-ship))))

; (def tag-team-range 5)
(def tag-team-range 9)
(def retreat-range 21.1)

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
  (when-let [enemy-ship (map/nearest-enemy-not-decoy ship enemy-ships)]
    (if (or (> *num-ships* ignore-retreating-ship-count)
            (> (math/distance-between ship enemy-ship) (get-retreat-range *num-ships*)))
      (move-ship-to-attack ship enemy-ship)
      ; (let [attack? (map/have-advantage? (custom-math/get-point-between ship enemy-ship 0.8))]
      (let [attack? (map/have-advantage? enemy-ship)]
      ; (let [attack? false]
        (if attack?
          ; (let [nearest-ship (nearest-ship)])
          (move-ship-to-attack ship enemy-ship)
          ; (if (map/alone? ship enemy-ship *player-id* tag-team-range false)
          (move-ship-to-retreat ship enemy-ship))))))
            ; (navigation/navigate-to-retreat ship target)))))))

(defn move-to-nearest-enemy-ship
  "Moves the ship to the nearest enemy ship."
  [ship enemy-ships]
  (when-let [enemy-ship (map/nearest-enemy-not-decoy ship enemy-ships)]
    (if (or (> *num-ships* ignore-retreating-ship-count)
            (> (math/distance-between ship enemy-ship) (get-retreat-range *num-ships*)))
      (move-ship-to-attack ship enemy-ship)
      (let [attack? (map/have-advantage? enemy-ship)]
      ; (let [attack? (map/have-advantage? (custom-math/get-point-between ship enemy-ship 0.8))]
      ; (let [attack? false]
        (if attack?
          (move-ship-to-attack ship enemy-ship)
          (if (map/alone? ship enemy-ship *player-id* tag-team-range false)
            (move-ship-to-retreat ship enemy-ship)
            (move-ship-to-retreat-for-real ship enemy-ship)))))))

(defn get-reachable-attack-spot-move
  "Figures out the move to make in order to move to an attack spot."
  [ship]
  nil)
  ; (when-let [best-spot (map/get-best-attack-spot ship)]
  ;   (log "I am trying to move to best spot" best-spot)
  ;   (let [move (navigation/navigate-to-friendly-ship ship best-spot)]
  ;     (when (and move (pos? (:thrust move)))
  ;       move))))

(defn compute-move-closest-planet*
  "Picks the move for the ship based on proximity to planets and fighters near planets."
  [{:keys [start-ms]} ship]
  (let [times-up? (> (- (System/currentTimeMillis) start-ms) 1550)]
    (if (or times-up?
            (not= :undocked (-> ship :docking :status)))
      nil
      (let [move (get-reachable-attack-spot-move ship)]
        (if move
          move
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
                         (map/safe-to-dock? ship))
                  (move-ship-to-planet! ship nearest-planet)
                  (move-to-nearest-enemy-ship ship (concat (vals *pesky-fighters*) (vals *docked-enemies*))))))))))))

(defn compute-move-closest-planet
  "Picks the move for the ship based on proximity to planets and fighters near planets."
  [{:keys [start-ms moving-ships] :as custom-map-info} ship]
  (when-not (some #{(:id ship)} moving-ships)
    (when-let [move (compute-move-closest-planet* custom-map-info ship)]
      (map/change-ship-positions! move)
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
                     (map/safe-to-dock? ship))
              (move-ship-to-planet! ship best-planet)
              (move-to-nearest-enemy-ship ship (concat (vals *pesky-fighters*) (vals *docked-enemies*))))))))))

(defn compute-move-best-planet
  "Picks the move for the ship based on proximity to planets and fighters near planets."
  [{:keys [start-ms moving-ships] :as custom-map-info} ship]
  (when-not (some #{(:id ship)} moving-ships)
    (when-let [move (compute-move-best-planet* custom-map-info ship)]
      (map/change-ship-positions! move)
      move)))

(def unprotected-distance
  "How far away we look for unprotected ships."
  56)

(defn- find-unprotected-ships
  "Helper function."
  [potential-ships assigned-ships]
  (for [enemy-ship (vals *docked-enemies*)
        ; :let [no-help (empty? (filter #(and (= (:owner-id enemy-ship (:owner-id %)))
        ;                                     (<= (math/distance-between % enemy-ship) 7))
        ;                               potential-ships))]
        :let [no-help (navigation/unreachable? enemy-ship (filter #(= (:owner-id enemy-ship)
                                                                      (:owner-id %))
                                                                  potential-ships))]
        :when no-help
        :let [closest-ship (map/nearest-entity enemy-ship
                                               (remove #(or (= (:id enemy-ship) (:id %))
                                                            ; (not= *player-id* (:id %))
                                                            (some (set [%]) @assigned-ships))
                                                       potential-ships))]
        :when (and closest-ship
                   (= *player-id* (:owner-id closest-ship))
                   (not (some #{closest-ship} @assigned-ships)))
        :let [distance (math/distance-between enemy-ship closest-ship)]
        :when (< distance unprotected-distance)]
    (do
      (swap! assigned-ships conj closest-ship)
      {:ship closest-ship
       :enemy-ship enemy-ship
       :distance distance})))

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
        all-permutations (find-unprotected-ships potential-ships assigned-ships)
        sorted-order (sort (utils/compare-by :distance utils/desc) all-permutations)]
    ;; This makes sure that if the same ship is the closest to multiple it picks the closest one.
    (into {}
      (for [triple-map sorted-order]
        [(:ship triple-map) (:enemy-ship triple-map)]))))

(defn attack-unprotected-enemy-ships
  "Returns moves to attack the unprotected enemy ships"
  [moving-ships {:keys [start-ms]}]
  (let [ship-attacks (unprotected-enemy-ships moving-ships)]
    ; (log "unprotected enemy ships are:" ship-attacks)
    (doall
     (for [[ship enemy-ship] ship-attacks
           :let [times-up? (> (- (System/currentTimeMillis) start-ms) 1550)]
           :when (not times-up?)
           :let [move (navigation/navigate-to-attack-docked-ship
                       ; ship enemy-ship true)]
                       ship enemy-ship (> (math/distance-between ship enemy-ship) 21.1))]
           :when move]
       (do (map/change-ship-positions! move)
           move)))))

(defn get-vulnerable-ships
  "Returns a list of vulnerable ships."
  [my-ships]
  (let [my-docked-ships (remove #(= :undocked (-> % :docking :status))
                                my-ships)
        my-fighter-ships (filter #(= :undocked (-> % :docking :status))
                                my-ships)
        vulnerable-distance 42
        ; vulnerable-distance 49
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
                                  my-ships)
        neutral-planet-count (count (filter #(nil? (:owner-id %))
                                            (vals *planets*)))]
    (when (and (> *num-players* 2)
               ;; Less than 10 percent of the total ships and no more than 2 neutral planets
               (<= neutral-planet-count 2)
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
  (let [potential-ships (filter #(and (= *player-id* (:owner-id %))
                                      (not (some (set [(:id %)]) moving-ships)))
                                (vals *ships*))
        max-defenders (* *num-ships* (/ 4 7))
        vulnerable-ships (take max-defenders (get-vulnerable-ships potential-ships))]
    (doall
     (for [[defender ship enemy] vulnerable-ships
           ; :let [advantage? (map/have-advantage? (custom-math/get-point-between ship enemy 0.8))
           :let [times-up? (> (- (System/currentTimeMillis) start-ms) 1550)]
           :when (not times-up?)
           :let [advantage? (map/have-advantage? enemy)
                 move (get-reachable-attack-spot-move ship)
                 move (if move
                        move
                        (navigation/navigate-to-defend-ship defender ship enemy advantage?))]
           :when move]
       (do (map/change-ship-positions! move)
           move)))))

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
          :let [times-up? (> (- (System/currentTimeMillis) start-ms) 1550)]
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

(defn-timed starting-game-strategy!
  "Function called at beginning of game before starting."
  []
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
    (when (> *num-players* 2)
      (reset! map/avoid-planets (map :id four-center-planets))
      (reset! center-planet/center-planets (map :id four-center-planets)))
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
        (map/change-ship-positions! move)
        (assoc move :subtype :retreat5)))))

(defn get-best-move
  "Returns the best move for the current ship and target planet."
  [start-ms ship target]
  (let [times-up? (> (- (System/currentTimeMillis) start-ms) 1550)]
    (if (or times-up?
            (not= :undocked (-> ship :docking :status)))
      nil
      (let [move (get-reachable-attack-spot-move ship)]
        (if move
          move
          (if (pos? @all-out-attack)
            (move-to-nearest-enemy-ship ship (concat (vals *pesky-fighters*) (vals *docked-enemies*)))

            (let [planets (filter #(e/any-remaining-docking-spots? %)
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
                         (map/safe-to-dock? ship))
                  (move-ship-to-planet! ship nearest-planet)
                  (move-to-nearest-enemy-ship-or-target ship (concat (vals *pesky-fighters*) (vals *docked-enemies*))
                                                        target))))))))))

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
    (for [{:keys [ship target]} (sort-fighter-move-map fighter-move-map pois)
          :let [move (get-best-move start-ms ship target)]
          :when move]
      (do
          ; (log "Move is" move)
          (map/change-ship-positions! move)
          move))))

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
                       (map/nearest-entity (assoc docking-spot :radius 0) potential-ships)
                       (map/nearest-entity planet potential-ships))]
    (when (and closest-ship
               (map/safe-to-dock? closest-ship))
      (when-let [move (move-ship-to-planet! closest-ship planet)]
        (do (map/change-ship-positions! move)
            move)))))

(defn get-best-planet-moves
  "Returns moves towards the best planet."
  [planet moving-ships]
  (if (and (not= 2 *num-players*)
           (<= @all-out-attack 0)
           (<= *num-ships* 3)
           planet
           (some #{(:id planet)} (keys *safe-planets*)))
    (return-planet-move planet moving-ships)
    (when (and planet (> *num-ships* 20))
      (return-planet-move planet moving-ships))))

(defn get-moves-and-moving-ships
  "TODO Returns moves and moving ships - make it easier to reorder."
  [moves-fn existing-moves moving-ships]
  nil)

(defn- process-swarm-moves
  "Processes swarm moves."
  [moves]
  (for [move moves]
    (do
     (map/change-ship-positions! move)
     move)))

(defn get-swarm-moves
  "Returns the swarm moves."
  [{:keys [start-ms]} ships]
  (let [retreat-range (get-retreat-range *num-ships*)
        ; enemy-ships (filter #(not= *player-id* (:owner-id %)) (vals *ships*))
        swarms (swarm/get-swarms ships)
        moves (for [single-swarm swarms
                    :let [times-up? (> (- (System/currentTimeMillis) start-ms) 1550)]
                    :when (not times-up?)
                    :let [enemy-ships (concat (vals *docked-enemies*) (vals *pesky-fighters*))
                          swarm-moves (when (seq enemy-ships)
                                        (swarm/get-swarm-move single-swarm enemy-ships retreat-range
                                                              *player-id*))]
                    :when (seq swarm-moves)]
                (do (process-swarm-moves swarm-moves)
                    swarm-moves))]
    ; (log "The swarm-moves " (pr-str moves))
    (flatten moves)))

(defn compute-planet-only-move*
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
            (when (and nearest-planet
                       (some #{(:id nearest-planet)} (keys *safe-planets*))
                       (map/safe-to-dock? ship))
              (move-ship-to-planet! ship nearest-planet))))))))

(defn compute-planet-only-move
  "Picks the move for the ship based on proximity to planets and fighters near planets."
  [{:keys [start-ms moving-ships] :as custom-map-info} ship]
  (when-not (some #{(:id ship)} moving-ships)
    (when-let [move (compute-planet-only-move* custom-map-info ship)]
      (map/change-ship-positions! move)
      move)))

(defn-timed get-planet-only-moves
  "Returns the planet only moves"
  [ships-in-order custom-map-info]
  (doall
   (keep #(compute-planet-only-move custom-map-info %)
         ships-in-order)))

; (defn get-best-planet
;   "Returns the best planet to take."
;   []
;   (if (= *num-players* 2)
;     (map/closest-dockable-planet)
;     (if (> *num-ships* 8)
;       (map/closest-dockable-planet)
;       (map/corner-planet))))

(defn get-best-planet
  "Returns the best planet to take."
  []
  (if (= *num-players* 2)
    (map/closest-planet-to-my-planets)
    (if (> *num-ships* 8)
      (map/closest-planet-to-my-planets)
      (map/corner-planet))))

(defn get-moves-for-turn
  "Returns all of the moves for this turn."
  [turn]
  ; (if (= 10 turn) (throw (Exception. "quit")))
  (let [custom-map-info (calculations-for-turn turn)
        ships-in-order (map/sort-ships-by-distance (vals (get *owner-ships* *player-id*)))
        runaway-moves (run-to-corner-moves (reverse ships-in-order))
        moving-ships (map #(get-in % [:ship :id]) runaway-moves)
        defend-moves (defend-vulnerable-ships moving-ships custom-map-info)
        moving-ships (map #(get-in % [:ship :id]) (concat defend-moves runaway-moves))

        attack-moves (attack-unprotected-enemy-ships moving-ships custom-map-info)
        moving-ships (map #(get-in % [:ship :id]) (concat runaway-moves attack-moves defend-moves))
        ; potential-ships (filter #(and (= :undocked (-> % :docking :status))
        ;                               (not (some (set [(:id %)]) moving-ships)))
        ;                         ships-in-order)
        ; swarm-moves (get-swarm-moves potential-ships)
        ; moving-ships (map #(get-in % [:ship :id]) runaway-moves swarm-moves defend-moves attack-moves)
        swarm-moves []
        best-planet (get-best-planet)
        best-planet-move (get-best-planet-moves best-planet moving-ships)
        best-planet-moves (if (seq best-planet-move) (flatten [best-planet-move]) [])
        moving-ships (map #(get-in % [:ship :id]) (concat runaway-moves attack-moves defend-moves best-planet-moves swarm-moves))
        more-planet-moves (get-planet-only-moves ships-in-order (assoc custom-map-info :moving-ships moving-ships))
        moving-ships (map #(get-in % [:ship :id]) (concat runaway-moves attack-moves defend-moves more-planet-moves best-planet-moves swarm-moves))

        ; potential-ships (filter #(and (= :undocked (-> % :docking :status))
        ;                               (not (some (set [(:id %)]) moving-ships)))
        ;                         ships-in-order)
        ; swarm-moves (if (> *num-ships* 3)
        ;               (get-swarm-moves custom-map-info potential-ships)
        ;               (get-swarm-moves custom-map-info potential-ships))
        ; moving-ships (map #(get-in % [:ship :id]) (concat runaway-moves swarm-moves defend-moves attack-moves best-planet-moves more-planet-moves))

        my-fighters (map/get-fighters *player-id* moving-ships)
        assigned-fighter-to-targets (map/fighters-to-targets my-fighters)
        main-moves (get-fighter-moves custom-map-info assigned-fighter-to-targets)
        moving-ships (map #(get-in % [:ship :id]) (concat runaway-moves attack-moves defend-moves main-moves best-planet-moves swarm-moves more-planet-moves))
        ; _ (log "Moving ships: " moving-ships)
        potential-ships (filter #(and (= :undocked (-> % :docking :status))
                                      (not (some (set [(:id %)]) moving-ships)))
                                ships-in-order)
        ; _ (log "potential ships: " potential-ships)
        swarm-moves (if (> *num-ships* 3)
                      (get-swarm-moves custom-map-info potential-ships)
                      (get-swarm-moves custom-map-info potential-ships))
                      ; [])
        moving-ships (map #(get-in % [:ship :id]) (concat runaway-moves swarm-moves defend-moves attack-moves main-moves best-planet-moves more-planet-moves))
        fallback-moves (get-main-moves ships-in-order (assoc custom-map-info :moving-ships moving-ships))
        all-moves (concat runaway-moves defend-moves attack-moves main-moves fallback-moves best-planet-moves swarm-moves more-planet-moves)
        friendly-moves (recalculate-friendly-moves (filter #(= :friendly (:subtype %)) all-moves) custom-map-info)

        ; _ (log "All moves" (pretty-log all-moves))
        ; _ (log "Friendly moves" (pretty-log friendly-moves))
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

    (concat all-moves friendly-moves)))
    ; (concat runaway-moves defend-moves attack-moves main-moves fallback-moves friendly-moves best-planet-moves swarm-moves more-planet-moves)))
