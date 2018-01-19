(ns custom.map-analysis
  "Functions for tracking custom info about the map."
  (:require
   [custom.game-map :refer [*docked-enemies* *pesky-fighters* *safe-planets* *num-ships*
                            *num-players* *spawn-points*]]
   [custom.math :as custom-math :refer [infinity]]
   [custom.utils :as utils :refer [pretty-log]]
   [custom.center-planet :as center-planet]
   [hlt.entity :as e]
   [hlt.game-map :refer [*player-id* *ships* *planets* *owner-ships* *map-size*]]
   [hlt.math :as math]
   [hlt.utils :refer [log]]))

(def best-planet
  (atom nil))

(defn nearest-entity
  "Returns the closest other entity to the passed in entity."
  [entity other-entities]
  ; (log "Called with entity" entity "Other entities" other-entities)
  (:nearest-entity
   (reduce (fn [{:keys [min-distance nearest-entity]} other-entity]
             (let [distance (- (math/distance-between entity other-entity)
                               (:radius other-entity))]
               (if (< distance min-distance)
                 {:min-distance distance :nearest-entity other-entity}
                 {:min-distance min-distance :nearest-entity nearest-entity})))
           {:min-distance infinity}
           other-entities)))

(defn farthest-entity
  "Returns the farthest other entity to the passed in entity."
  [entity other-entities]
  ; (log "Called with entity" entity "Other entities" other-entities)
  (:farthest-entity
   (reduce (fn [{:keys [max-distance farthest-entity]} other-entity]
             (let [distance (- (math/distance-between entity other-entity)
                               (:radius other-entity))]
               (if (> distance max-distance)
                 {:max-distance distance :farthest-entity other-entity}
                 {:max-distance max-distance :farthest-entity farthest-entity})))
           {:max-distance 0}
           other-entities)))

(defn find-nemesis-ships
  "Returns the player I should focus on battling."
  [planets]
  (let [my-planets (filter #(= *player-id* (:owner-id %)) planets)
        close-fighters (filter #(and (= :undocked (-> % :docking :status))
                                     (not= *player-id* (:owner-id %))
                                     (let [closest-planet (nearest-entity % my-planets)]
                                       (and closest-planet
                                            (< (- (math/distance-between % closest-planet)
                                                  (:radius closest-planet))
                                               30))))
                               (vals *ships*))
        fighters-by-owner-id (group-by :owner-id close-fighters)
        max-close-fighters (apply max 0 (map count (vals fighters-by-owner-id)))
        bad-guys (keep (fn [[k v]]
                         (when (= max-close-fighters (count v))
                           k))
                       fighters-by-owner-id)]
    (first bad-guys)))

(defn find-nemesis-planet
  "Returns the player I should focus on battling."
  [planets]
  (let [my-planets (filter #(= *player-id* (:owner-id %)) planets)
        enemy-planets (remove #(or (= *player-id* (:owner-id %))
                                   (nil? (:owner-id %)))
                              planets)
        closest-enemy-planet (atom nil)
        closest-distance (atom infinity)]
    (doseq [my-planet my-planets
            enemy-planet enemy-planets
            :let [distance (- (math/distance-between my-planet enemy-planet)
                              (:radius my-planet)
                              (:radius enemy-planet))]
            :when (< distance @closest-distance)]
      (do
       (reset! closest-enemy-planet enemy-planet)
       (reset! closest-distance distance)))
    (:owner-id @closest-enemy-planet)))

(def find-nemesis find-nemesis-planet)

(defn get-planet-values
  "Returns how valuable the planets are based on the current map."
  []
  nil)

(defn distance-to-poi
  "Returns the smallest distance to any POI."
  [ship points-of-interest]
  (reduce (fn [min-distance poi]
            (let [distance (- (math/distance-between ship poi) (get poi :radius 0.0))]
              (if (< distance min-distance)
                distance
                min-distance)))
          infinity
          points-of-interest))

(defn sort-ships-by-distance
  "Returns ships from closest to point of interest to farthest. A point of interest is a planet,
  docked enemy ship, enemy ship near one of my planets."
  [ships]
  (let [pois (concat (vals *docked-enemies*) (vals *safe-planets*) (vals *pesky-fighters*))
        ships-w-distance (map #(assoc % :distance (distance-to-poi % pois)) ships)]
    (mapv #(dissoc % :distance) (sort (utils/compare-by :distance utils/asc) ships-w-distance))))

(defn get-my-ships
  "Returns all of ships (including imaginary ships)."
  [ships]
  (filter #(= *player-id* (:owner-id %))
          ships))

(defn remove-imaginary-ships
  "Returns a list of all real ships"
  [ships]
  (vals
   (filter (fn [[k v]]
             (integer? k))
           ships)))

(defn get-my-real-ships
  "Returns my real ships"
  []
  (get-my-ships (remove-imaginary-ships *ships*)))

(defn get-fighters
  "Returns my fighter ships that aren't already moving."
  ([owner-id]
   (get-fighters owner-id []))
  ([owner-id moving-ships]
   (filter #(and (= owner-id (:owner-id %))
                 (= :undocked (-> % :docking :status))
                 (not (some (set [(:id %)]) moving-ships)))
           (remove-imaginary-ships *ships*))))

(defn sort-by-furthest
  "Sorts the passed in planets based on the furthest distance from any of the compared planets."
  [planets compared-planets]
  (when (and (seq planets)
             (seq compared-planets))
    (let [planet-maps (for [planet planets
                            :let [closest-planet (nearest-entity planet compared-planets)]
                            :when closest-planet
                            :let [distance (- (math/distance-between planet closest-planet)
                                              (:radius planet)
                                              (:radius closest-planet))]]
                        {:planet planet :distance distance})]
      (map :planet (sort (utils/compare-by :distance utils/desc) planet-maps)))))

(defn get-docked-enemy-ships
  "Returns all of the docked enemy ships."
  []
  (let [enemy-ships (remove #(= *player-id* (:owner-id %)) (vals *ships*))]
    (remove #(= :undocked (-> % :docking :status)) enemy-ships)))

(defn closest-useful-planet-distance
  "Returns the distance to the closest useful planet"
  [position planets]
  (if-let [nearest-planet (nearest-entity position planets)]
    (- (math/distance-between position nearest-planet) (:radius nearest-planet))
    0))

(def nemesis
  "My nemesis"
  (atom nil))

(defn get-enemy-planets
  "Returns the enemy planets."
  []
  (let [my-enemy @nemesis]
    (when my-enemy
      (filter (fn [planet]
                (= my-enemy (:owner-id planet)))
              (vals *planets*)))))

(defn nearest-enemy-not-decoy
  "Returns the closest enemy ship from the passed in enemy ships."
  [ship enemy-ships]
  (let [planets (get-enemy-planets)]
    (:nearest-ship
     (reduce (fn [{:keys [min-distance nearest-ship]} enemy]
               (let [distance-to-enemy (math/distance-between ship enemy)]
                 (if (< distance-to-enemy min-distance)
                   (let [before-distance (closest-useful-planet-distance ship planets)
                         after-distance (closest-useful-planet-distance enemy planets)]
                     (if (or (< after-distance (dec before-distance)) (< after-distance 7))
                       {:min-distance distance-to-enemy :nearest-ship enemy}
                       {:min-distance min-distance :nearest-ship nearest-ship}))
                   {:min-distance min-distance :nearest-ship nearest-ship})))
             {:min-distance infinity}
             enemy-ships))))

(defn good-surrounding-planet-helper
  "Checks whether we have the advantage for the docking spot at the given distance"
  [planet distance]
  (let [filter-fn (fn [ship]
                    (and (= :undocked (-> ship :docking :status))
                         (< (math/distance-between ship planet) (+ distance (:radius planet)))))
        docked-filter-fn (fn [ship]
                           (and (not= *player-id* (:owner-id ship))
                                (not= :undocked (-> ship :docking :status))
                                (< (math/distance-between ship planet) (+ distance (:radius planet)))))
        ships (for [[k v] *ships*
                    :when (integer? k)]
                v)
        closeby-docked (filter docked-filter-fn ships)
        nearby-fighters (filter filter-fn ships)
        fighters-by-owner (group-by :owner-id nearby-fighters)
        my-count (dec (count (get fighters-by-owner *player-id*)))
        max-other-count (apply max 0 (map count (vals (dissoc fighters-by-owner *player-id*))))
        ;; Special case to make sure I have one extra defender early
        my-count (if (and (< *num-ships* 5) (pos? max-other-count))
                   (dec my-count)
                   my-count)]
    (or (and (zero? max-other-count)
             (zero? (count closeby-docked)))
        (>= my-count max-other-count))))

(defn have-most-ships-surrounding-planet?
  "Have the most fighters (non docking) surrounding the planet."
  [planet]
  (let [owned? (= *player-id* (:owner-id planet))
        ;; Do some trickery here - consider a "planet" as the closest point to my closest ship
        nearest-ship (nearest-entity planet (filter #(and (= *player-id* (:owner-id %))
                                                          (= :undocked (-> % :docking :status)))
                                                    (vals *ships*)))
        planet (if nearest-ship
                 (assoc (math/closest-point nearest-ship planet) :radius 0.0)
                 planet)]
    ; (if (<= *num-ships* 3)
    ;   (good-surrounding-planet-helper planet 80)
      (if owned?
        (good-surrounding-planet-helper planet 25)
        (and
           ; (good-surrounding-planet-helper planet 80)
           (if (= *num-players* 2)
             (good-surrounding-planet-helper planet 80)
             true)
             ; (good-surrounding-planet-helper planet 70))
           (good-surrounding-planet-helper planet 45)
           (good-surrounding-planet-helper planet 30)
           (good-surrounding-planet-helper planet 15)))))
    ; (and (good-surrounding-planet-helper planet 60)
    ;      (good-surrounding-planet-helper planet 30)
    ;      (good-surrounding-planet-helper planet 15))))


(defn alone?
  "Returns true if I'm the only fighter nearby."
  [ship enemy-ship owner-id range docked?]
  (> 2
    (count
     (filter #(and (or (not docked?)
                       (= :undocked (-> % :docking :status)))
                   (or
                       (< (math/distance-between ship %) range)
                       (< (math/distance-between enemy-ship %) range)))
             (filter #(= *player-id* (:owner-id %))
                     (vals *ships*))))))

; (def advantage-range (* 2 (+ e/max-ship-speed e/ship-radius e/weapon-radius)))
(def advantage-range (* 1 (+ e/max-ship-speed e/ship-radius e/weapon-radius)))
; (def advantage-range (* 2 (+ e/max-ship-speed e/weapon-radius)))
; (def advantage-range (* 1.1 (+ e/max-ship-speed e/ship-radius e/weapon-radius)))
; (def advantage-range (+ e/max-ship-speed e/weapon-radius))

; (defn have-advantage?
;   "Returns true if I have more fighters at a given position than the enemy."
;   [position]
;   (let [filter-fn (fn [ship]
;                     (and (= :undocked (-> ship :docking :status))
;                          (< (math/distance-between ship position) advantage-range)))
;         ships (for [[k v] *ships*
;                     :when (integer? k)]
;                 v)
;         nearby-fighters (filter filter-fn ships)
;         my-fighter-count (count (filter #(= *player-id* (:owner-id %))
;                                         nearby-fighters))
;         enemy-count (- (count nearby-fighters) my-fighter-count)]
;     (or (zero? enemy-count)
;         (and (> my-fighter-count 1)
;              (> my-fighter-count enemy-count)))))

(def friendly-distance-moved 6)
(def max-speed-distance (+ e/max-ship-speed e/weapon-radius 1.0))
(def planet-fudge-factor 0.6)

(defn can-attack-spot?
  "Returns whether a ship can reach the spot (tries to take into account planets)."
  [ship point]
  (let [closest-attack-point (math/closest-point ship (assoc point :radius 0.0) 6.0)
        filter-fn-planets #(math/segment-circle-intersects? ship closest-attack-point % planet-fudge-factor)]
    (empty? (filter filter-fn-planets (vals *planets*)))))

(defn have-advantage?
  "Returns true if the place I'm going looks to be good."
  [point]
  (let [filter-fn (fn [ship]
                    (and (= :undocked (-> ship :docking :status))
                         (< (math/distance-between ship point) advantage-range)))
        ships (for [[k v] *ships*
                    :when (integer? k)]
                v)
        nearby-fighters (filter filter-fn ships)
        final-enemy-ships (filter #(and (not= *player-id* (:owner-id %))
                                        (< (math/distance-between point %) max-speed-distance)
                                        (can-attack-spot? % point))
                                  nearby-fighters)
        final-my-ships (filter #(and (= *player-id* (:owner-id %))
                                     (if (= 7 (:turn %))
                                        (< (math/distance-between point %) friendly-distance-moved)
                                        (and (< (math/distance-between point %) max-speed-distance)
                                             (can-attack-spot? % point))))
                               nearby-fighters)]
    (or (empty? final-enemy-ships)
        (> (count final-my-ships) (count final-enemy-ships)))))

(defn get-pesky-fighters-new
  "Fighters near my planets or neutral planets."
  []
  (let [mine-or-neutral-planets (filter #(or (nil? (:owner-id %))
                                             (= *player-id* (:owner-id %)))
                                        (vals *planets*))
        close-distance (if (<= *num-ships* 10) 130 21.1)]
    (set
      (for [planet mine-or-neutral-planets
            ship (vals *ships*)
            :when (and (not= *player-id* (:owner-id ship))
                       (= :undocked (-> ship :docking :status))
                       (< (math/distance-between ship planet) close-distance))]
        ship))))

(defn get-pesky-fighters
  "Fighters near my planets or neutral planets."
  []
  (filter #(and (not= *player-id* (:owner-id %))
                (= :undocked (-> % :docking :status)))
          (vals *ships*)))

(defn players-with-planets
  "Returns the number of players with planets"
  []
  (->> *planets*
       vals
       (filter :owner-id)
       (group-by :owner-id)
       keys
       count))

(def avoid-planets
  (atom nil))

(defn avoid-planet?
  "Returns whether we've decided to avoid this planet."
  [planet]
  (if (> (players-with-planets) 2)
    (some #{(:id planet)} @avoid-planets)))

(defn get-safe-planets
  "Returns a list of planets that are safe to dock at."
  []
  (let [filter-fn (fn [planet]
                    (and (or (nil? (:owner-id planet))
                             (and (= *player-id* (:owner-id planet))
                                  (e/any-remaining-docking-spots? planet)))
                         (have-most-ships-surrounding-planet? planet)
                         (not (avoid-planet? planet))))]
    (filter filter-fn (vals *planets*))))

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

(defn decorate-planets-with-values-four-players!
  "Gives each planet some value based on distance from other enemies."
  []
  (let [enemy-planets (remove #(or (nil? (:owner-id %))
                                   (= *player-id* (:owner-id %)))
                              (vals *planets*))
        planets-by-owner (group-by :owner-id enemy-planets)
        neutral-planets (filter #(or (nil? (:owner-id %))
                                     (and (= *player-id* (:owner-id %))
                                          (e/any-remaining-docking-spots? %)))
                                (vals *planets*))]
    (doseq [planet neutral-planets]
      (let [distances (map #(nearest-planet-distance planet %)
                           (vals planets-by-owner))
            square-value (reduce (fn [val next-val]
                                   (+ val (* next-val next-val)))
                                 0
                                 distances)
            square-value (Math/sqrt square-value)]
        ; (log "Square value for" planet "is" square-value)
        (set! *planets* (assoc-in *planets* [(:id planet) :square-value] square-value))))))

(def attack-spots
  (atom nil))

(defn get-turns-to-new-ship
  "Returns how many turns before a new ship will spawn for this docked ship's planet."
  [ship]
  (let [planet-id (-> ship :docking :planet)
        planet (*planets* planet-id)
        planet-progress (-> planet :docking :current-production)
        num-docked-ships (-> planet :docking :ships count)
        remaining-to-ship (- 72 planet-progress)]
    (int (/ remaining-to-ship (* num-docked-ships 6)))))

(defn get-turns-to-new-ship-planet
  "Returns how many turns before a new ship will spawn for this docked ship's planet."
  [planet]
  (let [planet-progress (-> planet :docking :current-production)
        num-docked-ships (-> planet :docking :ships count)
        remaining-to-ship (- 72 planet-progress)]
    (int (/ remaining-to-ship (* num-docked-ships 6)))))

(defn get-spawn-points
  "Modifies the enemy ships to include a pretend enemy at the spawn location for a planet."
  []
  (let [center-point (center-planet/get-center-point)
        enemy-planets (filter (fn [planet]
                                (and (not= *player-id* (:owner-id planet))
                                     (not (nil? (:owner-id planet)))))
                              (vals *planets*))]
    (for [planet enemy-planets
          :let [turns (get-turns-to-new-ship-planet planet)]
          :when (<= turns 1)]
      (math/closest-point center-point planet 2))))

(def players-in-order
  (atom []))

(defn avg
  "Returns the average of a collection of numbers"
  [coll]
  (/ (reduce + coll) (count coll)))

(defn closest-players-by-base
  "Returns the player-ids in order of closest to furthest from me."
  []
  (let [planets-by-owner (group-by :owner-id (remove #(or (nil? (:owner-id %))
                                                          (center-planet/center-planet? %))
                                                     (vals *planets*)))
        owner-bases (into {}
                          (for [[owner-id planets] planets-by-owner
                                :let [xs (map math/get-x planets)
                                      ys (map math/get-y planets)]]
                            [owner-id (math/->Position (avg xs) (avg ys))]))
        my-base (get owner-bases *player-id*)
        owner-distances (when my-base
                          (for [[owner-id base] (dissoc owner-bases *player-id*)]
                            {:owner-id owner-id
                             :distance (math/distance-between base my-base)}))
        sorted-owner-distances (sort (utils/compare-by :distance utils/asc) owner-distances)]
    (log "The closest players to me are: " sorted-owner-distances)
    (map :owner-id sorted-owner-distances)))

(defn docked-enemies-to-care-about
  "Returns the docked enemies I should try to go after."
  []
  (if (= 3 (count @players-in-order))
    (let [player-id (nth @players-in-order 2)]
      (log "Players order was" @players-in-order "and the player I'm going to attack is" player-id)
      (remove #(= player-id (:owner-id %)) (vals *docked-enemies*)))
    (vals *docked-enemies*)))

(def top-player-docked-ships
  (atom nil))

(defn find-docked-ships-without-an-army
  "Returns docked ships that don't have a bunch of fighters nearby."
  [fighter-ships]
  (let [army-number (max 4 (/ *num-ships* 10))
        army-distance 15
        docked-ships (docked-enemies-to-care-about)]
    (for [ship docked-ships
          :let [nearby-fighters (filter #(< (math/distance-between ship %) army-distance)
                                        fighter-ships)]
          :when (< (count nearby-fighters) army-number)]
      ship)))

(defn update-fighters
  "Updates fighter ships so I don't send too many to the same place."
  ([enemy-ship]
   (update-fighters enemy-ship 1))
  ([enemy-ship num-fighters]
   (let [fighter? (= :undocked (-> enemy-ship :docking :status))
         attack-count (+ num-fighters (get enemy-ship :attack-count 0))
         remove? (if fighter?
                   (>= attack-count 5)
                   (if (= (:owner-id enemy-ship) (first @players-in-order))
                      (>= attack-count (max 10 (/ *num-ships* 10)))
                      (if (= (:owner-id enemy-ship) (second @players-in-order))
                        (>= attack-count (max 10 (/ *num-ships* 10)))
                        (>= attack-count 3))))]

     (if fighter?
     ; (when fighter?
       (if remove?
         (set! *pesky-fighters* (dissoc *pesky-fighters* (:id enemy-ship)))
         (set! *pesky-fighters* (assoc-in *pesky-fighters* [(:id enemy-ship) :attack-count] attack-count)))
       (if remove?
         (do
           (reset! top-player-docked-ships (dissoc @top-player-docked-ships (:id enemy-ship)))
           (set! *docked-enemies* (dissoc *docked-enemies* (:id enemy-ship))))
         (do
           (when (get @top-player-docked-ships (:id enemy-ship))
             (reset! top-player-docked-ships (assoc-in @top-player-docked-ships [(:id enemy-ship) :attack-count] attack-count)))
           (set! *docked-enemies* (assoc-in *docked-enemies* [(:id enemy-ship) :attack-count] attack-count))))))))

(defn get-custom-map-info
  "Returns additional map info that is useful to calculate at the beginning of each turn."
  [turn]
  (let [start-ms (System/currentTimeMillis)
        all-ships-count (count (vals *ships*))]
    (decorate-planets-with-values-four-players!)
    (set! *safe-planets* (into {} (map (fn [planet] [(:id planet) planet]) (get-safe-planets))))
    (set! *docked-enemies* (into {} (map (fn [ship] [(:id ship) ship]) (get-docked-enemy-ships))))
    (set! *pesky-fighters* (into {} (map (fn [ship] [(:id ship) ship]) (get-pesky-fighters))))
    (set! *num-ships* (count (filter #(= *player-id* (:owner-id %)) (vals *ships*))))
    (set! *num-players* (count (filter (fn [[k v]]
                                         (seq v))
                                       *owner-ships*)))
    (set! *spawn-points* (get-spawn-points))
    (log "Spawn points:" *spawn-points*)
    (if (and (> *num-players* 2) (< *num-ships* (* 0.5 all-ships-count)))
      (reset! avoid-planets (deref center-planet/center-planets))
      (reset! avoid-planets nil))
    (log "Avoiding planets:" @avoid-planets)
    (reset! nemesis (find-nemesis (vals *planets*)))
    (reset! attack-spots nil)
    (reset! players-in-order (closest-players-by-base))
    (reset! top-player-docked-ships (into {} (map (fn [ship] [(:id ship) ship])
                                                  (find-docked-ships-without-an-army
                                                   (vals *pesky-fighters*)))))
    {:start-ms start-ms}))

(defn get-planets
  "Return all of the planets for the given owner ID."
  [owner-id]
  (filter #(= owner-id (:owner-id %))
          (vals *planets*)))

(defn fighters-to-targets
  "Returns a target for each of the fighters passed in."
  [ships]
  (when (seq ships)
    (when-let [enemy-planets (seq (sort-by-furthest (get-enemy-planets)
                                                    (or (seq (get-planets *player-id*))
                                                        [(first ships)])))]
      (let [num-per-planet (Math/ceil (/ (count ships) (count enemy-planets)))
            - (log "Num per planet" num-per-planet)
            _ (log "Enemy planets" enemy-planets)
            assigned-ships (atom nil)]
        (flatten
          (for [planet enemy-planets
                :let [my-ships (remove #(some (set [%]) @assigned-ships)
                                       ships)
                      sorted-ships (take num-per-planet (sort-by #(math/distance-between planet %)
                                                                 my-ships))]]
            (doall
             (for [ship sorted-ships]
              (do (swap! assigned-ships conj ship)
                  {:target planet :ship ship})))))))))

(defn get-unowned-planets
  "Returns unowned planets."
  []
  (filter #(nil? (:owner-id %)) (vals *planets*)))

(defn get-unowned-or-docked-good-planets
  "Returns unowned (and good) planets."
  []
  (filter #(and (or (nil? (:owner-id %))
                    (and (= *player-id* (:owner-id %))
                         (e/any-remaining-docking-spots? %)))
                (not (avoid-planet? %)))
          (vals *planets*)))

(def planet-distance-dock-spots 20)

(defn num-dock-slots-around-planet
  "Returns the number of docking spots within close range of a planet."
  [planet unowned-planets]
  (let [close-planets (filter (fn [other]
                                (< (- (math/distance-between planet other) (:radius planet) (:radius other))
                                   planet-distance-dock-spots))
                              unowned-planets)]
    (reduce + (map e/remaining-docking-spots close-planets))))

(defn planets-closest-to-me
  "Returns a list of planets that I am closest to."
  [unowned-planets]
  (filter (fn [planet]
            (let [closest-ship (nearest-entity planet (vals *ships*))]
              (= *player-id* (:owner-id closest-ship))))
          unowned-planets))

(defn safest-planet
  "Finds the planet that is the farthest away from every other player."
  []
  (let [unowned-planets (get-unowned-or-docked-good-planets)
        potential-planets (planets-closest-to-me unowned-planets)
        enemy-ships (filter #(not= *player-id* (:owner-id %))
                            (vals *ships*))
        planet-distances (for [planet potential-planets
                               :let [closest-ship (nearest-entity planet enemy-ships)
                                     distance (math/distance-between planet closest-ship)]]
                           {:planet planet
                            :distance distance})]
    (:planet (first (sort (utils/compare-by :distance utils/desc) planet-distances)))))

(defn corner-planet
  "Finds the planet that is the farthest away from the opposite corner player."
  []
  (let [unowned-planets (get-unowned-or-docked-good-planets)
        potential-planets (planets-closest-to-me unowned-planets)
        enemy-ships (filter #(not= *player-id* (:owner-id %))
                            (vals *ships*))
        planet-distances (for [planet potential-planets
                               :let [farthest-ship (farthest-entity planet enemy-ships)
                                     distance (math/distance-between planet farthest-ship)]]
                           {:planet planet
                            :distance distance})]
    (:planet (first (sort (utils/compare-by :distance utils/desc) planet-distances)))))

(defn corner-big-planet
  "Finds the biggest planet of the 3 furthest away planets."
  []
  (let [unowned-planets (get-unowned-or-docked-good-planets)
        potential-planets (planets-closest-to-me unowned-planets)
        enemy-ships (filter #(not= *player-id* (:owner-id %))
                            (vals *ships*))
        planet-distances (for [planet potential-planets
                               :let [farthest-ship (farthest-entity planet enemy-ships)
                                     distance (math/distance-between planet farthest-ship)]]
                           {:planet planet
                            :distance distance
                            :dock-spots (-> planet :docking :spots)})
        top-3 (take 3 (sort (utils/compare-by :distance utils/desc) planet-distances))]

    (:planet (first (sort (utils/compare-by :dock-spots utils/desc :distance utils/desc)
                          top-3)))))

(defn my-best-planet
  "Returns the planet that I should target."
  []
  (let [unowned-planets (get-unowned-or-docked-good-planets)
        potential-planets (planets-closest-to-me unowned-planets)
        planets-that-count (if (= 2 *num-players*)
                             unowned-planets potential-planets)
        planets-and-dock-spots (for [planet potential-planets
                                     :let [dock-spots (num-dock-slots-around-planet planet planets-that-count)]]
                                 {:planet planet :dock-spots dock-spots})
        sorted-list (sort (utils/compare-by :dock-spots utils/desc) planets-and-dock-spots)]
    (log "Best planet" (first sorted-list))
    (:planet (first sorted-list))))

; (defn dockable-planets
;   "Returns the planets I could dock at."
;   []
;   (let [planets (if (= 2 *num-players*)
;                   (vals *planets*)
;                   (remove avoid-planet? (vals *planets*)))
;         dockable-planets (filter #(nil? (:owner-id %))
;                                  planets)]
;     (if (seq dockable-planets)
;       dockable-planets
;       (filter #(and (= *player-id* (:owner-id %))
;                     (e/any-remaining-docking-spots? %))
;               planets))))

(defn dockable-planets
  "Returns the planets I could dock at."
  []
  (let [planets (if (= 2 *num-players*)
                  (vals *planets*)
                  (remove avoid-planet? (vals *planets*)))
        dockable-planets (filter #(or (nil? (:owner-id %))
                                      (and (= *player-id* (:owner-id %))
                                           (e/any-remaining-docking-spots? %)))
                                 planets)]
    dockable-planets))
    ; (if (seq dockable-planets)
    ;   dockable-planets
    ;   (filter #(and (= *player-id* (:owner-id %))
    ;                 (e/any-remaining-docking-spots? %))
    ;           planets))))

(defn closest-dockable-planet
  "Returns the closest planet I can dock at."
  []
  (let [planets (dockable-planets)
        my-fighters (filter #(and (= *player-id* (:owner-id %))
                                  (= :undocked (-> % :docking :status)))
                            (vals *ships*))
        planet-distances (for [planet planets
                               :let [closest-ship (nearest-entity planet my-fighters)]
                               :when closest-ship
                               :let [distance (- (math/distance-between planet closest-ship)
                                                 (:radius planet) (:radius closest-ship))]]
                           {:planet planet
                            :distance distance})]
    (:planet (first (sort (utils/compare-by :distance utils/asc) planet-distances)))))

(defn most-isolated-planet
  "Returns the planet the furthest away from enemy planets."
  []
  (let [enemy-planets (filter #(and (not (nil? (:owner-id %)))
                                    (not= *player-id* (:owner-id %)))
                              (vals *planets*))
        planets (dockable-planets)
        planet-distances (for [planet planets
                               :let [closest-enemy-planet (nearest-entity planet enemy-planets)]
                               :when closest-enemy-planet
                               :let [distance (- (math/distance-between planet closest-enemy-planet)
                                                 (:radius closest-enemy-planet) (:radius planet))]]
                            {:planet planet
                             :distance distance})]
    (:planet (first (sort (utils/compare-by :distance utils/desc) planet-distances)))))

(defn closest-planet-to-my-planets
  "Returns the planet the furthest away from enemy planets."
  []
  (let [my-planets (filter #(= *player-id* (:owner-id %))
                           (vals *planets*))
        planets (dockable-planets)
        planet-distances (for [planet planets
                               :let [closest-planet (nearest-entity planet my-planets)]
                               :when closest-planet
                               :let [distance (- (math/distance-between planet closest-planet)
                                                 (:radius closest-planet) (:radius planet))]]
                            {:planet planet
                             :distance distance})]
    (:planet (first (sort (utils/compare-by :distance utils/asc) planet-distances)))))

(defn players-with-planets
  "Returns the number of players with planets"
  []
  (->> *planets*
       vals
       (filter :owner-id)
       (group-by :owner-id)
       keys
       count))

(defn reachable?
  "Returns true if I can reach the passed-in location from current spot."
  [current-spot target-spot]
  (< (math/distance-between current-spot target-spot) e/max-ship-speed))

(defn get-best-attack-spot
  "Returns the best attack spot for the current ship."
  [ship]
  (first (filter #(reachable? ship %) @attack-spots)))

; (defn calculate-end-positions
;   "Returns a ship in its end position based on thrust for this turn."
;   [{:keys [ship thrust angle] :as move}]
;   (let [x (get-in ship [:pos :x])
;         y (get-in ship [:pos :y])
;         positions (map math/map->Position
;                        (custom-math/all-positions-start-to-end x y thrust angle))]
;     (map #(assoc ship :pos %) positions)))

(defn change-ship-positions!
  "Changes a ships position in the main ships."
  [{:keys [ship type subtype thrust] :as move}]
  ; (log "Trying to build imaginary ships for:" (pretty-log move))
  (when (and (= :thrust type) (pos? thrust))
    (let [imaginary-ships (custom-math/calculate-end-positions move)]
      ; (log "IS:" (pretty-log imaginary-ships))
      (doseq [i-ship (conj (butlast imaginary-ships) (assoc ship :turn 1))]
        (set! *ships* (assoc *ships* (java.util.UUID/randomUUID) i-ship)))
      (when (last imaginary-ships)
        ; (log "Last imaginary-ship is:" (last imaginary-ships))
        (set! *ships* (assoc *ships* (:id ship) (last imaginary-ships)))))))

(defn player-to-avoid
  "Returns the player to avoid (or nil if there is none)"
  []
  (when (= 3 (count @players-in-order))
    (nth @players-in-order 2)))

; (defn docked-enemies-to-care-about
;   "Returns the docked enemies I should try to go after."
;   []
;   (if-let [player-id (player-to-avoid)]
;     (do
;      (log "Players order was" @players-in-order "and the player I'm going to ignore is" player-to-avoid)
;      (remove #(= player-id (:owner-id %)) (vals *docked-enemies*)))
;     (vals *docked-enemies*)))

(def close-fighter-distance 55)

(defn safe-to-dock?
  "Returns true if I have a defender nearby."
  [ship]
  (let [all-fighters (filter #(and (= :undocked (-> % :docking :status))
                                   (not= (:id ship) (:id %)))
                             (vals *ships*))
        closest-fighter (nearest-entity ship all-fighters)]
    (or (nil? closest-fighter)
        (= *player-id* (:owner-id closest-fighter))
        (> (math/distance-between ship closest-fighter) close-fighter-distance))))
