(ns custom.durbinator
  (:require
   [custom.game-map :refer [*safe-planets* *docked-enemies* *pesky-fighters* *num-ships*]]
   [custom.math :as custom-math]
   [custom.navigation :as navigation]
   [custom.utils :as utils :refer [defn-timed]]
   [hlt.entity :as e]
   [hlt.game-map :refer [*player-id* *map-size* *bot-name* *ships* *planets*]]
   [hlt.math :as math]
   [hlt.utils :refer [log]])
  (:import (java.io PrintWriter)))

(def my-bot-name "Durbinator")

(def infinity 99999999)

; (defn coin-flip-navigation-options
;   "Randomly returns navigation options from one of two choices."
;   []
;   ; (if (= 0 (rand-int 2))
;   navigation/default-navigation-opts)
;     ; navigation/reverse-nagivation-opts])

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

(defn nearest-enemy-ship
  "Returns the closest enemy ship from the passed in enemy ships."
  [ship enemy-ships]
  (:nearest-ship
   (reduce (fn [{:keys [min-distance nearest-ship]} enemy]
             (let [distance-to-enemy (math/distance-between ship enemy)]
               (if (< distance-to-enemy min-distance)
                 {:min-distance distance-to-enemy :nearest-ship enemy}
                 {:min-distance min-distance :nearest-ship nearest-ship})))
           {:min-distance infinity}
           enemy-ships)))

(defn get-docked-enemy-ships
  "Returns all of the docked enemy ships."
  []
  (let [enemy-ships (remove #(= *player-id* (:owner-id %)) (vals *ships*))]
    (remove #(= :undocked (-> % :docking :status)) enemy-ships)))

(defn move-ship-to-attack
  "Moves the ship to attack the enemy ship."
  [ship enemy-ship]
  (if (= :undocked (-> enemy-ship :docking :status))
    (navigation/navigate-to-attack-ship ship enemy-ship)
    (navigation/navigate-to-attack-docked-ship ship enemy-ship)))

(def retreat-if-this-close 35)

(defn move-ship-to-retreat
  "Moves the ship to retreat from the enemy ship."
  [ship enemy-ship]
  (let [my-ships (filter #(= *player-id* (:owner-id %))
                         (vals *ships*))
        my-other-ships (remove #(= (:id ship) (:id %))
                               my-ships)
        closest-friendly-ship (nearest-enemy-ship ship my-other-ships)]
    (if (and closest-friendly-ship
             (< (math/distance-between closest-friendly-ship ship) retreat-if-this-close))
      (navigation/navigate-to-friendly-ship ship closest-friendly-ship)
      (navigation/navigate-to-retreat ship enemy-ship))))

(def tag-team-range 8)
(def retreat-range 24)

(defn alone?
  "Returns true if I'm the only fighter nearby."
  [ship enemy-ship owner-id range docked?]
  (> 2
    (count
     (filter #(and (or (not docked?)
                       (= :undocked (-> % :docking :status)))
                   (or (< (math/distance-between ship %) range)
                       (< (math/distance-between enemy-ship %) range)))
             (filter #(= *player-id* (:owner-id %))
                     (vals *ships*))))))

(def ignore-retreating-ship-count
  "Optimization to not worry about running away when I have this many ships."
  55)

(defn-timed move-to-nearest-enemy-ship
  "Moves the ship to the nearest enemy ship."
  [ship enemy-ships]
  (when-let [enemy-ship (nearest-enemy-ship ship enemy-ships)]
    (if (or (> *num-ships* ignore-retreating-ship-count)
            (not= :undocked (-> enemy-ship :docking :status))
            (> (math/distance-between ship enemy-ship) retreat-range)
            (not (alone? ship enemy-ship *player-id* tag-team-range false)))
            ; (alone? ship enemy-ship (:owner-id enemy-ship) (* 5 tag-team-range) true))
      (move-ship-to-attack ship enemy-ship)
      (move-ship-to-retreat ship enemy-ship))))

(defn get-pesky-fighters
  "Fighters near my planets or neutral planets."
  []
  (let [mine-or-neutral-planets (filter #(or (nil? (:owner-id %))
                                             (= *player-id* (:owner-id %)))
                                        (vals *planets*))
        close-distance 56]
    (set
      (for [planet mine-or-neutral-planets
            ship (vals *ships*)
            :when (and (not= *player-id* (:owner-id ship))
                       (= :undocked (-> ship :docking :status))
                       (< (math/distance-between ship planet) close-distance))]
        ship))))

(defn-timed have-most-ships-surrounding-planet?
  "Have the most fighters (non docking) surrounding the planet."
  [planet]
  (let [close-distance 39
        filter-fn (fn [ship]
                    (and (= :undocked (-> ship :docking :status))
                         (< (math/distance-between ship planet) (+ close-distance (:radius planet)))))
        docked-filter-fn (fn [ship]
                           (and (not= *player-id* (:owner-id ship))
                                (not= :undocked (-> ship :docking :status))
                                (< (math/distance-between ship planet) (+ close-distance (:radius planet)))))
        ships (vals *ships*)
        closeby-docked (filter docked-filter-fn ships)
        nearby-fighters (filter filter-fn ships)
        fighters-by-owner (group-by :owner-id nearby-fighters)
        my-count (dec (count (get fighters-by-owner *player-id*)))
        max-other-count (apply max 0 (map count (vals (dissoc fighters-by-owner *player-id*))))]
    (or
        (>= my-count (max 1 (* 2 max-other-count)))
        (>= my-count (+ 2 max-other-count))
        (and (zero? max-other-count) (zero? (count closeby-docked))))))

(defn get-safe-planets
  "Returns a list of planets that are safe to dock at."
  []
  (let [filter-fn (fn [planet]
                    (and (or (nil? (:owner-id planet))
                             (and (= *player-id* (:owner-id planet))
                                  (e/any-remaining-docking-spots? planet)))
                         (have-most-ships-surrounding-planet? planet)))]
    (filter filter-fn (vals *planets*))))

(defn compute-move-closest-planet*
  "Picks the move for the ship based on proximity to planets and fighters near planets."
  [{:keys [start-ms]} ship]
  (let [times-up? (> (- (System/currentTimeMillis) start-ms) 1550)]
    (if (or times-up?
            (not= :undocked (-> ship :docking :status)))
      nil
      (when-let [planets (filter #(or (not= *player-id* (:owner-id %))
                                      (e/any-remaining-docking-spots? %))
                                 (vals *planets*))]
        (let [nearest-planet
              (:nearest-planet
               (reduce (fn [{:keys [min-distance nearest-planet]} planet]
                         (let [distance-to-planet (math/distance-between ship planet)]
                           (if (< distance-to-planet min-distance)
                             {:min-distance distance-to-planet :nearest-planet planet}
                             {:min-distance min-distance :nearest-planet nearest-planet})))
                       {:min-distance infinity}
                       planets))]
          (if (and nearest-planet
                   (some #{(:id nearest-planet)} (keys *safe-planets*)))
            (move-ship-to-planet! ship nearest-planet)
            (move-to-nearest-enemy-ship ship (concat *pesky-fighters* *docked-enemies*))))))))

(defn calculate-end-positions
  "Returns a ship in its end position based on thrust for this turn."
  [{:keys [ship thrust angle] :as move}]
  (let [x (get-in ship [:pos :x])
        y (get-in ship [:pos :y])
        ; positions [(math/map->Position (custom-math/final-position x y thrust angle))]
        positions (map math/map->Position
                       (custom-math/all-positions-start-to-end x y thrust angle))]
    (map #(assoc ship :pos %) positions)))

; (defn change-ship-positions!
;   "Changes a ships position in the main ships."
;   [{:keys [ship type subtype] :as move}]
;   (when (and (= :thrust type) (not= :friendly subtype))
;     (when-let [imaginary-ships (seq (calculate-end-positions move))]
;       (doseq [i-ship (butlast imaginary-ships)]
;         (set! *ships* (assoc *ships* (java.util.UUID/randomUUID) i-ship)))
;       (set! *ships* (assoc *ships* (:id ship) (last imaginary-ships))))))

(defn change-ship-positions!
  "Changes a ships position in the main ships."
  [{:keys [ship type subtype] :as move}]
  ;  (change-ship-positions! move true)
  ; ([{:keys [ship type subtype] :as move} ignore-friendly?]
  (when (and (= :thrust type)
            (not= :friendly subtype))
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

(defn-timed get-custom-map-info
  "Returns additional map info that is useful to calculate at the beginning of each turn."
  []
  (set! *safe-planets* (into {} (map (fn [planet] [(:id planet) planet]) (get-safe-planets))))
  (set! *docked-enemies* (get-docked-enemy-ships))
  (set! *pesky-fighters* (get-pesky-fighters))
  (set! *num-ships* (count (filter #(= *player-id* (:owner-id %)) (vals *ships*))))
  {:start-ms (System/currentTimeMillis)})

(defn distance-to-poi
  "Returns the smallest distance to any POI."
  [ship points-of-interest]
  (reduce (fn [min-distance poi]
            (let [distance-to-poi (math/distance-between ship poi)]
              (if (< distance-to-poi min-distance)
                distance-to-poi
                min-distance)))
          infinity
          points-of-interest))

(def unprotected-distance
  "How far away we look for unprotected ships."
  55)

(defn unprotected-enemy-ships
  "Returns a map of unprotected enemy ships with my ship as the key and the enemy ship as the value.
  TODO - this will only find a single dock point most of the time because the same ship of mine
  is probably the closest to several spots. Remove my ship each time it is selected. Will require
  several passes."
  [moving-ships]
  ; (log "Moving ships are" moving-ships)
  (let [potential-ships (filter #(and (= :undocked (-> % :docking :status))
                                      (not (some (set [(:id %)]) moving-ships)))
                                (vals *ships*))
        all-permutations
         (for [enemy-ship *docked-enemies*
               :let [closest-ship (nearest-enemy-ship enemy-ship
                                                     (remove #(= (:id enemy-ship) (:id %))
                                                             potential-ships))]
               :when (= *player-id* (:owner-id closest-ship))
               :let [distance (math/distance-between enemy-ship closest-ship)]
               :when (< distance unprotected-distance)]
           {:ship closest-ship
            :enemy-ship enemy-ship
            :distance distance})
        sorted-order (sort (utils/compare-by :distance utils/desc) all-permutations)]
    ;; This makes sure that if the same ship is the closest to multiple it picks the closest one.
    (into {}
      (for [triple-map sorted-order]
        [(:ship triple-map) (:enemy-ship triple-map)]))))

(defn-timed attack-unprotected-enemy-ships
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
  []
  (let [my-ships (filter #(= *player-id* (:owner-id %)) (vals *ships*))
        my-docked-ships (remove #(= :undocked (-> % :docking :status))
                                my-ships)
        my-fighter-ships (filter #(= :undocked (-> % :docking :status))
                                my-ships)
        vulnerable-distance 16
        all-permutations
         (for [ship my-docked-ships
               ;; We should break out as soon as we find a single close enemy
               :let [enemy-ship (nearest-enemy-ship ship *pesky-fighters*)]
               :when enemy-ship
               :let [enemy-distance (math/distance-between enemy-ship ship)]
               :when (< enemy-distance vulnerable-distance)
               :let [my-closest-ship (nearest-enemy-ship ship my-fighter-ships)]
               :when my-closest-ship]
           {:ship my-closest-ship
            :vulnerable-ship ship
            :distance enemy-distance})
        sorted-order (sort (utils/compare-by :distance utils/desc) all-permutations)]
    ;; This makes sure that if the same ship is the closest to multiple it picks the closest one.
    (into {}
      (for [triple-map sorted-order]
        [(:ship triple-map) (:vulnerable-ship triple-map)]))))

(defn-timed defend-vulnerable-ships
  "Returns moves to defend vulnerable ships."
  []
  (let [vulnerable-ships (get-vulnerable-ships)]
    (log "Vulnerable ships " vulnerable-ships)
    (doall
     (for [[defender ship] vulnerable-ships
           :let [move (navigation/navigate-to-friendly-ship defender ship)]
           :when move]
       (do (change-ship-positions! move)
           move)))))

(defn-timed sort-ships-by-distance
  "Returns ships from closest to point of interest to farthest. A point of interest is a planet,
  docked enemy ship, enemy ship near one of my planets."
  [ships]
  (let [pois (concat *docked-enemies* (vals *safe-planets*) *pesky-fighters*)
        ships-w-distance (map #(assoc % :distance (distance-to-poi % pois)) ships)]
    (mapv #(dissoc % :distance) (sort (utils/compare-by :distance utils/asc) ships-w-distance))))

(defn recalculate-friendly-moves
  "Returns the retreat to nearby friendly moves now that we know where all the other ships are
  going."
  [moves]
  (let [my-ships (filter #(= *player-id* (:owner-id %))
                         (vals *ships*))]
    (for [move moves
          :let [;; Have to recalculate every turn in order to get the newly added imaginary ships - no I shouldn't :()
                ; my-ships (filter #(= *player-id* (:owner-id %))
                ;                  (vals *ships*))
                ship (:ship move)
                my-other-ships (remove #(= (:id ship) (:id %))
                                       my-ships)
                closest-friendly-ship (nearest-enemy-ship ship my-other-ships)
                new-move (navigation/navigate-to-friendly-ship ship closest-friendly-ship)]
          :when new-move]
      (do (change-ship-positions! (assoc new-move :subtype :friendly2))
          new-move))))

(defn-timed get-planet-values
  "Returns how valuable the planets are based on the current map."
  []
  nil)
