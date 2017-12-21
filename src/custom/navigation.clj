(ns custom.navigation
  (:require
   [custom.math :as custom-math :refer [infinity]]
   [custom.utils :refer [defn-timed]]
   [hlt.entity :as e]
   [hlt.utils :refer [log]]
   [hlt.navigation :as hlt-navigation]
   [hlt.game-map :refer [*planets* *ships* *player-id* *map-size*]]
   [hlt.math :as math :refer [get-x get-y]]))

(def default-navigation-opts
  (assoc hlt-navigation/default-navigation-opts
         :max-corrections 360 :buffer 0 :avoid-attack true))

(def reverse-nagivation-opts
  (assoc default-navigation-opts :angular-step (/ Math/PI -180.0)))

(defn valid-point?
  "Returns true if the point is on the map."
  [point]
  (let [[max-x max-y] *map-size*
        x (get-x point)
        y (get-y point)]
    (and (< 0.5 x (dec max-x))
         (< 0.5 y (dec max-y)))))

(def potential-obstacle-distance
  (+ e/max-ship-speed 2.2 0.6))

(defn figure-out-potential-obstacles
  "TODO: Note planets I should look further out, but ships this is fine."
  [ship all-ships]
  (filter #(<= (math/distance-between ship %) potential-obstacle-distance) all-ships))

(def slightly-smaller-fudge-factor 0.6)
(def planet-fudge-factor 0.6)
(def swarm-fudge-factor 1.8)

; (defn entities-between-filter-fn
;   "Returns the function to use for the entities-between-filter-fn."
;   (fn [entity]
;     (and (distinct? a b %)
;          (math/segment-circle-intersects? a b % slightly-smaller-fudge-factor))))

(defn new-entities-between
  "More efficient entities-between"
  [a b obstacles]
  (let [filter-fn #(math/segment-circle-intersects? a b % slightly-smaller-fudge-factor)
        filter-fn-planets #(math/segment-circle-intersects? a b % planet-fudge-factor)]
    (concat (filter filter-fn-planets (vals *planets*))
            (filter filter-fn obstacles))))

(defn swarm-entities-between
  "More efficient entities-between"
  [a b obstacles]
  (let [filter-fn #(math/segment-circle-intersects? a b % swarm-fudge-factor)
        filter-fn-planets #(math/segment-circle-intersects? a b % swarm-fudge-factor)]
    (concat (filter filter-fn-planets (vals *planets*))
            (filter filter-fn obstacles))))

(comment
  (count all-navigation-iterations))

(def all-navigation-iterations
  "Returns the angular-step and max thrust for each potential navigation iteration."
  (for [iterations (range 1 6)
        thrust [7 6 4 2]
        ; thrust [7 6 5 4 3 2 1]
        angular-step (range 30)
        opposite (range 2)
        ; :let [angular-step (* 2 (/ Math/PI 180.0) angular-step)]
        :let [angular-step (* 1 (/ Math/PI 180.0) angular-step)]]
    {:max-thrust thrust
     :angular-step (if (zero? opposite) (* -1 iterations angular-step) (* iterations angular-step))}))

(def safe-radius
  "How far away a spot is guaranteed to be safe."
  (+ e/max-ship-speed (* 2 e/ship-radius) e/weapon-radius 0.6))
  ; (+ e/max-ship-speed (* 2 e/ship-radius) e/weapon-radius))

(defn unreachable?
  "Returns true if none of the passed in ships can attack this position on the map."
  [position ships]
  (not (some? (seq (filter #(< (math/distance-between position %) safe-radius) ships)))))

; (defn really-close?
;   "Returns true if there's a ."
;   [position ships]
;   (some? (seq (filter #(< (math/distance-between position %) safe-radius) ships))))

(defn not-guaranteed-safe?
  "Returns true if we're less than 5 away from a ship."
  [ship ships]
  (some? (first (filter #(<= (math/distance-between ship %) 7) ships))))

(defn navigate-to
  "Returns a thrust move that moves the ship to the provided goal. The
  goal is treated as a point, i.e. the thrust move attempts to move
  the ship to the center of the goal. Use navigate-to-dock to compute
  a thrust move that does not collide with entities, or use
  closest-point yourself to find a suitable point. This function
  returns nil if it cannot find a suitable path."
  ([ship goal]
   (navigate-to ship goal default-navigation-opts))
  ([ship goal {:keys [max-corrections avoid-obstacles
                      angular-step max-thrust buffer subtype avoid-attack]
               :as opts}]
   (let [distance (math/distance-between ship goal)
         first-angle (math/orient-towards ship goal)
         obstacles (figure-out-potential-obstacles ship (remove #(or (= (:id ship) (:id %))
                                                                     (= (:id goal) (:id %)))
                                                                (vals *ships*)))
         other-ships (when avoid-attack
                       (remove #(or (= *player-id* (:owner-id %))
                                    (not= (:undocked (-> % :docking :status))))
                               obstacles))
         avoid-attack (if (and avoid-attack (unreachable? ship other-ships))
                        avoid-attack
                        false)
         thrust (int (min (- distance buffer) max-thrust))]
     (if (< distance buffer)
       (e/thrust-move ship 0 first-angle)
       (loop [iterations (rest all-navigation-iterations)]
         (let [{:keys [max-thrust angular-step]} (first iterations)]
           (if (nil? max-thrust)
             (e/thrust-move ship 0 first-angle)
             (let [angle (+ first-angle angular-step)
                   point (custom-math/get-point ship (min max-thrust thrust) angle)]
               (if (or (not (valid-point? point))
                       (and avoid-attack (not (unreachable? point other-ships)))
                       (and avoid-obstacles (first (new-entities-between ship point obstacles))))
                 (recur (rest iterations))
                 (assoc (e/thrust-move ship (min max-thrust thrust) angle) :subtype subtype))))))))))

(defn navigate-swarm-to
  "Returns a thrust move that moves the ship to the provided goal. The
  goal is treated as a point, i.e. the thrust move attempts to move
  the ship to the center of the goal. Use navigate-to-dock to compute
  a thrust move that does not collide with entities, or use
  closest-point yourself to find a suitable point. This function
  returns nil if it cannot find a suitable path."
  ([ship goal]
   (navigate-to ship goal default-navigation-opts))
  ([ship goal {:keys [max-corrections avoid-obstacles
                      angular-step max-thrust buffer subtype avoid-attack]
               :as opts}]
   (let [distance (math/distance-between ship goal)
         first-angle (math/orient-towards ship goal)
         ids (map :id (:swarm ship))
         obstacles (figure-out-potential-obstacles ship (remove #(or (some (set [(:id %)]) ids)
                                                                     (= (:id goal) (:id %)))
                                                                (vals *ships*)))
         other-ships (when avoid-attack
                       (remove #(or (= *player-id* (:owner-id %))
                                    (not= (:undocked (-> % :docking :status))))
                               obstacles))
         avoid-attack (if (and avoid-attack (unreachable? ship other-ships))
                        avoid-attack
                        false)
         thrust (int (min (- distance buffer) max-thrust))]
     (log "My ship is " ship)
     (log "My goal is " goal)
     (log "My distance to go is " distance)
     (if (< distance buffer)
       (for [single-ship (:swarm ship)]
         (assoc (e/thrust-move single-ship 0 first-angle)
                :subtype subtype))
       (loop [iterations (rest all-navigation-iterations)]
         (let [{:keys [max-thrust angular-step]} (first iterations)]
           (if (nil? max-thrust)
             (for [single-ship (:swarm ship)]
               (assoc (e/thrust-move single-ship 0 first-angle)
                      :subtype subtype))
             (let [angle (+ first-angle angular-step)
                   point (custom-math/get-point ship (min max-thrust thrust) angle)]
               (if (or (not (valid-point? point))
                       (and avoid-attack (not (unreachable? point other-ships)))
                       (and avoid-obstacles (first (swarm-entities-between ship point obstacles))))
                 (recur (rest iterations))
                 ;; One move for each ship in the swarm
                 (for [single-ship (:swarm ship)
                       :let [single-angle (math/orient-towards single-ship point)
                             single-thrust (min max-thrust (int (math/distance-between single-ship
                                                                                       point)))]]
                   (assoc (e/thrust-move single-ship (min max-thrust single-thrust) single-angle)
                          :subtype subtype)))))))))))

(defn navigate-to-specific-point
  "Navigate to a specific point"
  [ship goal]
  (navigate-to ship goal (merge default-navigation-opts {:buffer 0 :avoid-attack false :subtype :point})))

(defn navigate-to-attack-ship
  "Navigate to with a buffer to not crash into ship."
  ([ship goal]
   (navigate-to-attack-ship ship goal false))
  ([ship goal avoid-attack?]
   (navigate-to ship goal
                (merge default-navigation-opts {:buffer 3.5
                                                :subtype :attack
                                                :avoid-attack avoid-attack?}))))
                                                ; :avoid-attack false}))))

(defn navigate-swarm-to-attack-ship
  "Navigate to with a buffer to not crash into ship."
  [ship goal]
  (navigate-swarm-to ship goal
                     (merge default-navigation-opts {:buffer 3.5
                                                     :subtype :swarm-attack
                                                     :avoid-attack false})))

(defn navigate-to-attack-docked-ship
  "Navigate to with a buffer to not crash into ship."
  ([ship goal]
   (navigate-to-attack-docked-ship ship goal false))
  ([ship goal avoid-attack?]
   (navigate-to ship goal (merge default-navigation-opts {
                                                          ; :buffers 1.1
                                                          :buffer 4.5
                                                          :subtype :docked-attack
                                                          ; :avoid-attack false
                                                          :avoid-attack avoid-attack?}))))

(def retreat-iterations 180)
(def retreat-angular-step (/ 360 retreat-iterations))

(defn navigate-to-retreat-ship
  "Attempt to retreat and pull the ships away from my planets. Pick four points and make sure
  I cannot be attacked from them."
  [ship goal]
  (let [ships (remove #(or (= *player-id* (:owner-id %))
                           (not= :undocked (-> % :docking :status)))
                      (vals *ships*))
        not-safe (not-guaranteed-safe? ship ships)]
    (loop [angle (custom-math/orient-away ship goal)
           iteration 0]
      (let [goal (custom-math/get-point ship e/max-ship-speed angle)]
        (if (or not-safe (unreachable? goal ships))
          (navigate-to ship goal (merge default-navigation-opts {:buffer 0.0
                                                                 :max-thrust e/max-ship-speed
                                                                 :subtype :retreat-ship}))
          (if (<= iteration retreat-iterations)
            (recur (mod (+ retreat-angular-step angle) 360)
                   (inc iteration))))))))

(defn navigate-swarm-to-retreat-ship
  "Attempt to retreat and pull the ships away from my planets. Pick four points and make sure
  I cannot be attacked from them."
  [ship goal]
  (let [ships (remove #(or (= *player-id* (:owner-id %))
                           (not= :undocked (-> % :docking :status)))
                      (vals *ships*))
        not-safe (not-guaranteed-safe? ship ships)]
    (loop [angle (custom-math/orient-away ship goal)
           iteration 0]
      (let [goal (custom-math/get-point ship e/max-ship-speed angle)]
        (if (or not-safe (unreachable? goal ships))
          (navigate-swarm-to ship goal (merge default-navigation-opts {:buffer 1.1
                                                                       :max-thrust e/max-ship-speed
                                                                       :subtype :swarm-retreat}))
          (if (<= iteration retreat-iterations)
            (recur (mod (+ retreat-angular-step angle) 360)
                   (inc iteration))))))))


(defn navigate-to-swarm-ship
  "Returns a thrust move which will navigate this ship to the requested
  planet for docking. The ship will attempt to get to
  `docking-distance` units above the planet's surface. Returns nil if
  it cannot find a suitable path."
  [ship friendly-ship]
  (navigate-to ship friendly-ship (merge default-navigation-opts {:buffer 1.1
                                                                  :subtype :swarm-friendly})))
                                                                  ; :max-corrections 10})))

(defn navigate-to-friendly-ship-later
  "Return a fake move to be processed later."
  [ship friendly-ship]
  (assoc (e/thrust-move ship 0 0) :subtype :friendly))

(defn navigate-to-friendly-ship
  "Returns a thrust move which will navigate this ship to the requested
  planet for docking. The ship will attempt to get to
  `docking-distance` units above the planet's surface. Returns nil if
  it cannot find a suitable path."
  [ship friendly-ship]
  (navigate-to ship friendly-ship (merge default-navigation-opts {:buffer 1.1
                                                                  :subtype :friendly2})))
                                                                  ; :max-corrections 10})))

(defn navigate-to-defend-ship
  "Returns a thrust move which will navigate this ship to the requested
  planet for docking. The ship will attempt to get to
  `docking-distance` units above the planet's surface. Returns nil if
  it cannot find a suitable path."
  [ship friendly-ship enemy-ship advantage?]
  (let [angle (math/orient-towards friendly-ship enemy-ship)
        distance (math/distance-between friendly-ship enemy-ship)
        ;; Try to prevent sending my ship in to die
        distance (if advantage?
                   (/ (* 2 distance) 3)
                   0)
        midpoint (custom-math/get-point friendly-ship distance angle)]
    ; (if (and (not advantage?) (< distance 7))
    ;   (navigate-to ship (custom-math/get-point friendly-ship 7 angle)
    ;                (merge default-navigation-opts {:buffer 0.0 :subtype :suicide}))
    (navigate-to ship midpoint (merge default-navigation-opts {:buffer 1.1
                                                               :subtype :defend}))))

(defn navigate-to-dock
  "Returns a thrust move which will navigate this ship to the requested
  planet for docking. The ship will attempt to get to
  `docking-distance` units above the planet's surface. Returns nil if
  it cannot find a suitable path."
  [ship planet]
  (let [docking-point (math/closest-point ship planet hlt-navigation/docking-distance)]
    (navigate-to ship docking-point (assoc default-navigation-opts :subtype :dock))))

(def too-close-distance 0.3)

(defn too-close-to-planet
  "Returns true if the point is < than 3 units away from a planet."
  [position planet]
  (let [distance-from-planet (- (math/distance-between position planet) (:radius planet))]
    (< distance-from-planet too-close-distance)))

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

(defn navigate-to-retreat
  "Attempt to retreat and pull the ships away from my planets and towards their own.
  I cannot be attacked from them."
  [ship planet]
  (let [ships (remove #(or (= *player-id* (:owner-id %))
                           (not= :undocked (-> % :docking :status)))
                      (vals *ships*))
        orig-planet planet
        not-safe (not-guaranteed-safe? ship ships)
        orig-angle (if not-safe
                     (custom-math/orient-away ship (nearest-entity ship ships))
                     (math/orient-towards ship planet))]
    (loop [angle orig-angle
           iteration 0]
      (let [planet (custom-math/get-point ship e/max-ship-speed angle)]
        (if (and (or not-safe (unreachable? planet ships))
                 (not (too-close-to-planet planet orig-planet)))
          (navigate-to ship planet (merge default-navigation-opts {:buffer 0.0
                                                                   :max-thrust e/max-ship-speed
                                                                   :subtype :retreat3}))
          (when (<= iteration retreat-iterations)
            (let [angle (if (even? iteration)
                          (mod (+ (* retreat-angular-step (int (/ iteration 2))) orig-angle) 360)
                          (mod (+ (* retreat-angular-step (int (/ (inc iteration) 2)) -1) orig-angle) 360))]
              (recur angle
                     (inc iteration)))))))))

(defn navigate-swarm-to-retreat
  "Attempt to retreat and pull the ships away from my planets and towards their own.
  I cannot be attacked from them."
  [ship planet]
  (let [ships (remove #(or (= *player-id* (:owner-id %))
                           (not= :undocked (-> % :docking :status)))
                      (vals *ships*))
        orig-planet planet
        not-safe (not-guaranteed-safe? ship ships)
        orig-angle (if not-safe
                     (custom-math/orient-away ship (nearest-entity ship ships))
                     (math/orient-towards ship planet))]
    (loop [angle orig-angle
           iteration 0]
      (let [planet (custom-math/get-point ship e/max-ship-speed angle)]
        (if (and (or not-safe (unreachable? planet ships))
                 (not (too-close-to-planet planet orig-planet)))
          (navigate-swarm-to ship planet (merge default-navigation-opts {:buffer 0.0
                                                                         :max-thrust e/max-ship-speed
                                                                         :subtype :swarm-retreat3}))
          (when (<= iteration retreat-iterations)
            (let [angle (if (even? iteration)
                          (mod (+ (* retreat-angular-step (int (/ iteration 2))) orig-angle) 360)
                          (mod (+ (* retreat-angular-step (int (/ (inc iteration) 2)) -1) orig-angle) 360))]
              (recur angle
                     (inc iteration)))))))))

(defn closest-position
  "Returns closest position from the current position to a collection of positions."
  [ship positions]
  (:nearest-pos
   (reduce (fn [{:keys [min-distance nearest-pos]} position]
             (let [distance-to-position (math/distance-between ship position)]
               (if (< distance-to-position min-distance)
                 {:min-distance distance-to-position :nearest-pos position}
                 {:min-distance min-distance :nearest-pos nearest-pos})))
           {:min-distance infinity}
           positions)))

(defn navigate-to-nearest-corner
  "Navigate to the nearest corner for this stupid strategy everyone is doing."
  [position]
  (let [[max-x max-y] *map-size*
        ne (math/->Position (dec max-x) 0.1)
        nw (math/->Position 0.1 0.1)
        se (math/->Position (dec max-x) (dec max-y))
        sw (math/->Position 0.1 (dec max-y))]
    (navigate-to-retreat position (assoc (closest-position position [ne nw se sw]) :radius 0))))
