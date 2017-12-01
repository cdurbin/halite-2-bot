(ns custom.map-analysis
  "Functions for tracking custom info about the map."
  (:require
   [custom.game-map :refer [*docked-enemies* *pesky-fighters* *safe-planets* *num-ships*
                            *num-players*]]
   [custom.math :refer [infinity]]
   [custom.utils :as utils]
   [hlt.entity :as e]
   [hlt.game-map :refer [*player-id* *ships* *planets* *owner-ships*]]
   [hlt.math :as math]
   [hlt.utils :refer [log]]))

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

(defn find-nemesis
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

(defn get-planet-values
  "Returns how valuable the planets are based on the current map."
  []
  nil)

(defn distance-to-poi
  "Returns the smallest distance to any POI."
  [ship points-of-interest]
  (reduce (fn [min-distance poi]
            (let [distance (- (math/distance-between ship poi) (get poi :radius 0))]
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
  []
  (filter #(= *player-id* (:owner-id %))
          (vals *ships*)))

(defn remove-imaginary-ships
  "Returns a list of all real ships"
  []
  (vals
   (filter (fn [[k v]]
             (integer? k))
           *ships*)))

(defn get-fighters
  "Returns my fighter ships that aren't already moving."
  [owner-id moving-ships]
  ; (log "Ships:" *ships*)
  ; (log "Remove imaginary:" (remove-imaginary-ships))
  (filter #(and (= owner-id (:owner-id %))
                (= :undocked (-> % :docking :status))
                (not (some (set [(:id %)]) moving-ships)))
          (remove-imaginary-ships)))

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

(defn get-enemy-planets
  "Returns the enemy planets."
  []
  (let [my-enemy (find-nemesis (vals *planets*))]
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

(defn have-most-ships-surrounding-planet?
  "Have the most fighters (non docking) surrounding the planet."
  [planet]
  (let [
        ; close-distance (if (< *num-ships* 6) 60 35)
        close-distance 60
        filter-fn (fn [ship]
                    (and (= :undocked (-> ship :docking :status))
                         (< (math/distance-between ship planet) (+ close-distance (:radius planet)))))
        docked-filter-fn (fn [ship]
                           (and (not= *player-id* (:owner-id ship))
                                (not= :undocked (-> ship :docking :status))
                                (< (math/distance-between ship planet) (+ close-distance (:radius planet)))))
        ships (for [[k v] *ships*
                    :when (integer? k)]
                v)
        closeby-docked (filter docked-filter-fn ships)
        nearby-fighters (filter filter-fn ships)
        fighters-by-owner (group-by :owner-id nearby-fighters)
        my-count (dec (count (get fighters-by-owner *player-id*)))
        max-other-count (apply max 0 (map count (vals (dissoc fighters-by-owner *player-id*))))]
    ; (log "Planet" planet)
    (or (and (zero? max-other-count)
             (zero? (count closeby-docked)))
        (and
          (> my-count (+ 2 max-other-count))
          (let [close-distance 15
                filter-fn (fn [ship]
                            (and (= :undocked (-> ship :docking :status))
                                 (< (math/distance-between ship planet) (+ close-distance (:radius planet)))))
                docked-filter-fn (fn [ship]
                                   (and (not= *player-id* (:owner-id ship))
                                        (not= :undocked (-> ship :docking :status))
                                        (< (math/distance-between ship planet) (+ close-distance (:radius planet)))))
                ; closeby-docked (filter docked-filter-fn ships)
                nearby-fighters (filter filter-fn ships)
                fighters-by-owner (group-by :owner-id nearby-fighters)
                my-count (dec (count (get fighters-by-owner *player-id*)))
                max-other-count (apply max 0 (map count (vals (dissoc fighters-by-owner *player-id*))))]
            ; (>= my-count (+ 3 max-other-count))
            (>= my-count (max 1 (* 2 max-other-count))))))))

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

(def advantage-range (* 2 (+ e/max-ship-speed e/ship-radius e/weapon-radius)))

(defn have-advantage?
  "Returns true if I have more fighters at a given position than the enemy."
  [position]
  (let [filter-fn (fn [ship]
                    (and (= :undocked (-> ship :docking :status))
                         (< (math/distance-between ship position) advantage-range)))
        ships (for [[k v] *ships*
                    :when (integer? k)]
                v)
        ; nearby-fighters (filter filter-fn (vals (filter (fn [[k v]]
        ;                                                   (integer? k))
        ;                                                 *ships*)))
        nearby-fighters (filter filter-fn ships)
        ; _ (log "Nearby fighters are" nearby-fighters)
        my-fighter-count (count (filter #(= *player-id* (:owner-id %))
                                        nearby-fighters))
        enemy-count (- (count nearby-fighters) my-fighter-count)]
    (or (zero? enemy-count)
        (and (> my-fighter-count 1)
             (> my-fighter-count enemy-count)))))

(defn get-pesky-fighters-orig
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

(defn get-pesky-fighters
  "Fighters near my planets or neutral planets."
  []
  (filter #(and (not= *player-id* (:owner-id %))
                (= :undocked (-> % :docking :status)))
          (vals *ships*)))

(defn get-safe-planets
  "Returns a list of planets that are safe to dock at."
  []
  (let [filter-fn (fn [planet]
                    (and (or (nil? (:owner-id planet))
                             (and (= *player-id* (:owner-id planet))
                                  (e/any-remaining-docking-spots? planet)))
                         (have-most-ships-surrounding-planet? planet)))]
    (filter filter-fn (vals *planets*))))

(defn get-safe-planets-new
  "Returns a list of planets that are safe to dock at."
  []
  (let [filter-fn (fn [planet]
                    (and (or (nil? (:owner-id planet))
                             (and (= *player-id* (:owner-id planet))
                                  (e/any-remaining-docking-spots? planet)))
                         (or (> *num-ships* 5)
                             (have-most-ships-surrounding-planet? planet))))]
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

(defn get-custom-map-info
  "Returns additional map info that is useful to calculate at the beginning of each turn."
  [turn]
  (let [start-ms (System/currentTimeMillis)]
    (decorate-planets-with-values-four-players!)
    (set! *safe-planets* (into {} (map (fn [planet] [(:id planet) planet]) (get-safe-planets))))
    (set! *docked-enemies* (into {} (map (fn [ship] [(:id ship) ship]) (get-docked-enemy-ships))))
    (set! *pesky-fighters* (into {} (map (fn [ship] [(:id ship) ship]) (get-pesky-fighters))))
    (set! *num-ships* (count (filter #(= *player-id* (:owner-id %)) (vals *ships*))))
    (set! *num-players* (count (keys *owner-ships*)))
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
            ; remaining (- (count ships) (* num-per-planet (count enemy-planets)))
            assigned-ships (atom nil)]
        (flatten
          (for [planet enemy-planets
                :let [my-ships (remove #(some (set [%]) @assigned-ships)
                                       ships)
                      ; _ (log "CDD: assigned" @assigned-ships)
                      ; _ (log "remaining:" my-ships)
                      sorted-ships (take num-per-planet (sort-by #(math/distance-between planet %)
                                                                 my-ships))]]
            (doall
             (for [ship sorted-ships]
              (do (swap! assigned-ships conj ship)
                  ; (log "I just swapped" ship "- assigned" @assigned-ships)
                  {:target planet :ship ship})))))))))
      ; (for [planet enemy-planets
      ;       :let [my-ships (remove #(some (set [%]) @assigned-ships)
      ;                              ships)
      ;             my-ship (map/nearest-entity planet my-ships)]
      ;       :when my-ship
      ;       :let [move (navigation/navigate-to-retreat my-ship planet)]
      ;       :when move]
      ;   (do
      ;     (swap! assigned-ships conj my-ship)
      ;     (change-ship-positions! move)
      ;     (assoc move :subtype :retreat5)))))

;; remaining-docking-spots

(defn get-unowned-planets
  "Returns unowned planets."
  []
  (filter #(nil? (:owner-id %)) (vals *planets*)))

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

(defn my-best-planet
  "Returns the planet that I should target."
  []
  (let [unowned-planets (get-unowned-planets)
        potential-planets (planets-closest-to-me unowned-planets)
        planets-and-dock-spots (for [planet potential-planets
                                     :let [dock-spots (num-dock-slots-around-planet planet unowned-planets)]]
                                 {:planet planet :dock-spots dock-spots})
        sorted-list (sort (utils/compare-by :dock-spots utils/desc) planets-and-dock-spots)]
    (log "Best planet" (first sorted-list))
    (:planet (first sorted-list))))
