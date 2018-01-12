(ns custom.navigation
  (:require
   [custom.math :as custom-math :refer [infinity]]
   [custom.utils :refer [defn-timed pretty-log]]
   [hlt.entity :as e]
   [hlt.utils :refer [log]]
   [hlt.navigation :as hlt-navigation]
   [hlt.game-map :refer [*planets* *ships* *player-id* *map-size*]]
   [custom.game-map :refer [*num-ships* *start-ms*]]
   [hlt.math :as math :refer [get-x get-y]]))

(def default-navigation-opts
  (assoc hlt-navigation/default-navigation-opts
         :max-corrections 2 :buffer 0 :avoid-attack true
         :avoid-corner true))

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

; (def potential-obstacle-distance
;   (+ e/max-ship-speed 2.2 0.6))

(def potential-obstacle-distance
  ; (+ e/max-ship-speed 2.001)
  (+ e/max-ship-speed 1.01))

(def potential-attack-distance
  (+ (* 2 e/max-ship-speed) 6.5))
  ; (+ e/max-ship-speed 2.2 0.6))

(defn figure-out-potential-attacks
  "TODO: Note planets I should look further out, but ships this is fine."
  [ship all-ships]
  (filter #(and (not= *player-id* (:owner-id %))
                (= :undocked (-> % :docking :status))
                (<= (math/distance-between ship %) potential-attack-distance))
          all-ships))

(defn figure-out-potential-obstacles
  "TODO: Note planets I should look further out, but ships this is fine."
  [ship all-ships]
  (filter #(<= (math/distance-between ship %) potential-obstacle-distance) all-ships))

(defn figure-out-potential-obstacles-new
  "TODO: Note planets I should look further out, but ships this is fine."
  [ship all-ships]
  (filter (fn [[k v]] (<= (math/distance-between ship v) potential-obstacle-distance))
          all-ships))

; (def slightly-smaller-fudge-factor 0.6)
(def slightly-smaller-fudge-factor 0.6)
; (def midturn-fudge-factor 0.71)
(def midturn-fudge-factor 0.6)
; (def planet-fudge-factor 0.6)
(def planet-fudge-factor 0.6)
(def swarm-fudge-factor 1.8)

; (defn entities-between-filter-fn
;   "Returns the function to use for the entities-between-filter-fn."
;   (fn [entity]
;     (and (distinct? a b %)
;          (math/segment-circle-intersects? a b % slightly-smaller-fudge-factor))))

(defn new-entities-between
  "More efficient entities-between"
  [a b obstacles planet-point]
  (let [filter-fn #(math/segment-circle-intersects? a b % slightly-smaller-fudge-factor)
        filter-fn-planets #(math/segment-circle-intersects? a planet-point % planet-fudge-factor)]
        ; filter-fn-planets #(math/segment-circle-intersects? a b % planet-fudge-factor)]
    (concat (filter filter-fn-planets (vals *planets*))
            (filter filter-fn obstacles))))

(defn ship-entities-between
  "Ship entities between."
  [a b obstacles]
  (let [filter-fn #(math/segment-circle-intersects? a b % midturn-fudge-factor)]
    (filter filter-fn obstacles)))

(defn swarm-entities-between
  "More efficient entities-between"
  [a b obstacles]
  (let [filter-fn #(math/segment-circle-intersects? a b % swarm-fudge-factor)
        filter-fn-planets #(math/segment-circle-intersects? a b % swarm-fudge-factor)]
    (concat (filter filter-fn-planets (vals *planets*))
            (filter filter-fn obstacles))))

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

(comment
  (count all-navigation-iterations))

; (def all-navigation-iterations
;   "Returns the angular-step and max thrust for each potential navigation iteration."
;   (for [iterations (range 1 6)
;         ; thrust [7 6 4 2]
;         thrust [7 6 4 2 1]
;         ; thrust [7]
;         ; thrust [7 6 5 4 3 2 1]
;         angular-step (range 30)
;         opposite (range 2)
;         ; :let [angular-step (* 2 (/ Math/PI 180.0) angular-step)]
;         :let [angular-step (* 1 (/ Math/PI 180.0) angular-step)]]
;     {:max-thrust thrust
;      :angular-step (if (zero? opposite) (* -1 iterations angular-step) (* iterations angular-step))}))

(def all-navigation-iterations
  "Returns the angular-step and max thrust for each potential navigation iteration."
  ; (for [iterations (range 0 4.1)])
  (for [iterations (range 0 5.1)
        ; thrust [7 6 4 2]
        thrust [7 6 4 2 1]
        ; thrust [7]
        ; thrust [7 6 5 4 3 2 1]
        angle-slice (range 30)
        opposite (range 2)
        ; :let [angular-step (* 2 (/ Math/PI 180.0) angular-step)]
        :let [angular-step (* (/ Math/PI 180.0)
                              (+ angle-slice (* iterations 30)))]]
    {:max-thrust thrust
     :angular-step (if (zero? opposite)
                     (* -1 angular-step)
                     angular-step)}))

(def safe-radius
  "How far away a spot is guaranteed to be safe."
  13.51)
  ; (+ e/max-ship-speed (* 2 e/ship-radius) e/weapon-radius 0.6)
  ; (+ e/max-ship-speed (* 2 e/ship-radius) e/weapon-radius 0.6)
  ; (+ e/max-ship-speed (* 2 e/ship-radius) e/weapon-radius 0.6))
  ; (+ e/max-ship-speed (* 2 e/weapon-radius) (* 2 e/ship-radius)))
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
  (some? (first (filter #(<= (math/distance-between ship %) 6.0) ships))))
  ; (some? (first (filter #(<= (math/distance-between ship %) 5.1) ships))))

; (defn midturn-collisions
;   "Returns all of the midturn collisions that are possible.
;    locations - key of turn number and value the x,y position.
;    midturn-ships - a collection of ships which include the turn number when the ship is there."
;   [locations point midturn-ships]
;   (flatten
;    (for [location locations
;          :let [collisions (ship-entities-between location point
;                                                  (filter #(= (:turn %) (:turn location))
;                                                          midturn-ships))]
;          :when (seq collisions)]
;      collisions)))

(defn midturn-collisions
  "Returns all of the midturn collisions that are possible.
   locations - key of turn number and value the x,y position.
   midturn-ships - a collection of ships which include the turn number when the ship is there."
  [locations midturn-ships]
  ; (log (pretty-log midturn-ships))
  (flatten
   (for [[turn location] locations
         :when (:start location)
         :let [
               ; _ (log "Turn " turn)
               ; _ (log "Location" location)
               collisions (ship-entities-between (:start location) (:end location)
                                                 ; (filter #(= (inc turn) (:turn %))
                                                 ;         midturn-ships)
                                                 (filter #(<= (- turn 1)
                                                              (:turn %)
                                                              (+ turn 1))
                                                 ; (filter #(= turn (:turn %))
                                                         midturn-ships))]
         :when (seq collisions)]
     collisions)))

(def planet-compare-distance 27)

(def close-corner-distance 5)

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

(defn too-close-to-corner?
  "Returns true if the point is < than 3 units away from a corner."
  [position]
  ; (log "Called too close to corner for " position)
  (let [[max-x max-y] *map-size*
        ne (math/->Position (dec max-x) 0.1)
        nw (math/->Position 0.1 0.1)
        se (math/->Position (dec max-x) (dec max-y))
        sw (math/->Position 0.1 (dec max-y))]
    (first (filter #(< (math/distance-between position %) close-corner-distance)
                  (map (fn [point] (assoc point :radius 0.0)) [ne nw se sw])))))

(def friendly-distance-moved 2.5)
(def max-speed-distance (+ e/max-ship-speed e/weapon-radius 1.5))

(defn can-attack-spot?
  "Returns whether a ship can reach the spot (tries to take into account planets)."
  [ship point]
  (let [closest-attack-point (math/closest-point ship (assoc point :radius 0.0) 6)
        filter-fn-planets #(math/segment-circle-intersects? ship closest-attack-point % planet-fudge-factor)]
    (empty? (filter filter-fn-planets (vals *planets*)))))

(defn good-spot?
  "Returns true if the place I'm going looks to be good."
  [point my-ships enemy-ships]
  (let [final-enemy-ships (filter #(and (< (math/distance-between point %) max-speed-distance)
                                        (or (can-attack-spot? % point)
                                            (< (math/distance-between point %) (/ max-speed-distance 2))))

                                  ; (filter #(= :undocked (-> % :docking :status))
                                  enemy-ships)]
    ; (log "Good-spot:" point "My ships" my-ships "Their orig ships:" enemy-ships
    ;      "Their final ships:" (count final-enemy-ships))
    (or (empty? final-enemy-ships)
        (let [final-my-ships (filter #(if (or (= 7 (:turn %))
                                              (not= :undocked (-> % :docking :status)))
                                        (< (math/distance-between point %) friendly-distance-moved)
                                        (and (< (math/distance-between point %) max-speed-distance)
                                             (can-attack-spot? % point)))
                                     my-ships)]
          ; (log "Good-spot:" point "My ships" (count final-my-ships) "Their ships:" (count final-enemy-ships))
          (>= (count final-my-ships) (count final-enemy-ships))))))

(defn get-angle-to-run
  "Returns the angle to run away from an enemy."
  [ship enemies]
  (when-let [closest-enemy (nearest-entity ship enemies)]
    (custom-math/orient-away ship closest-enemy)))

(defn navigate-to-friendly-ship-later
  "Return a fake move to be processed later."
  [ship]
  (assoc (e/thrust-move ship 0 0) :subtype :friendly))

(defn navigate-to-precise
  "Returns a thrust move that moves the ship to the provided goal. The
  goal is treated as a point, i.e. the thrust move attempts to move
  the ship to the center of the goal. Use navigate-to-dock to compute
  a thrust move that does not collide with entities, or use
  closest-point yourself to find a suitable point. This function
  returns nil if it cannot find a suitable path."
  ([ship goal]
   (navigate-to-precise ship goal default-navigation-opts))
  ([ship goal {:keys [max-corrections avoid-obstacles
                      angular-step max-thrust buffer subtype avoid-attack avoid-corner]
               :as opts}]
   ; (log "Ship" ship)
   ; (log "Goal" goal)
   (let [
         buffer 0
         distance (math/distance-between ship goal)
         first-angle (math/orient-towards ship goal)
         ;          compare-ships (remove (fn [[k v]]
         ;                                   (or (= (:id ship) (:id v))
         ;                                       (= (:id goal) k)))
         ;                                       ; (not= *player-id* (:owner-id v))))
         ;                                *ships*)
         ;          obstacles (figure-out-potential-obstacles ship (vals compare-ships))
         potential-ships (remove (fn [[k v]]
                                   (or (= (:id ship) (:id v))))
                                       ; (= (:id goal) k)))
                                 *ships*)
         all-obstacles (figure-out-potential-obstacles-new ship potential-ships)
         final-ships-to-compare (filter (fn [[k v]]
                                          (integer? k))
                                          ; (nil? (:turn v)))
                                        all-obstacles)
         midturn-ships (remove (fn [[k v]]
                                 (integer? k))
                               all-obstacles)
         ; _ (when (= 6 (:id ship)) (log "MT SHIPS: " midturn-ships))
         my-ships (vals (filter (fn [[k v]]
                                  (and (integer? k)
                                       (= *player-id* (:owner-id v))))
                                potential-ships))
         other-ships (figure-out-potential-attacks ship (vals potential-ships))
         ; ship-attack-obstacles (figure-out-potential-attacks ship
         ;                                                     (filter #(not= *player-id* (:owner-id %))
         ;                                                             (vals potential-ships)))
         ; other-ships (when avoid-attack
         ;               (remove #(or (= *player-id* (:owner-id %))
         ;                            (not= :undocked (-> % :docking :status)))
         ;                       (vals all-obstacles)))
         thrust (int (min (- distance buffer) max-thrust))
         first-spot (custom-math/get-point ship thrust first-angle)
         ; avoid-attack (not (good-spot? first-spot my-ships other-ships))
         good-spot (good-spot? first-spot my-ships other-ships)
         guaranteed-safe (when-not good-spot
                           (not (not-guaranteed-safe? ship other-ships)))
         thrust (if (not good-spot)
                  7
                  thrust)
         ; first-angle (if (and (not good-spot) (not guaranteed-safe))
         ;               (let [closest-fighter]
         ;                 (nearest-entity)))
         avoid-attack (if good-spot
                         false
                         guaranteed-safe)
         first-angle (if (and (not good-spot) (not guaranteed-safe))
                       (or (get-angle-to-run ship other-ships) first-angle)
                       first-angle)
         _ (log "Avoid attack for: " (:id ship) "is" avoid-attack "good spot" good-spot "guaranteed-safe" guaranteed-safe)
         navigation-iterations (if (< thrust 7)
                                 (filter #(<= (:max-thrust %) thrust)
                                         all-navigation-iterations)
                                 all-navigation-iterations)]

         ; avoid-attack (if (and avoid-attack (not (not-guaranteed-safe? ship other-ships)))
         ;                avoid-attack
         ;                false)
     (if (and (not= :friendly2) (not good-spot))
       (navigate-to-friendly-ship-later ship)
       (if (< thrust buffer)
         (assoc (e/thrust-move ship 0 first-angle) :subtype subtype :reason "Precise - distance is less than buffer.")
         (loop [iterations (rest navigation-iterations)]
               ;; Timeout protection
           (let [times-up? (> (- (System/currentTimeMillis) *start-ms*) 1850)
                 {:keys [max-thrust angular-step]} (first iterations)]
             (if (or times-up? (nil? max-thrust))
               (assoc (e/thrust-move ship 0 first-angle) :subtype subtype :reason "Precise - ran out of moves.")
               (let [angle (+ first-angle angular-step)
                     ; point (custom-math/get-point ship (min max-thrust thrust) angle)
                     point (custom-math/get-point ship max-thrust angle)
                     planet-compare-point (custom-math/get-point ship (min max-thrust planet-compare-distance) angle)
                     midturn-locations (custom-math/get-in-turn-segments
                                        {:ship ship :thrust max-thrust
                                         :angle angle})]
                     ; _ (log "Starting point" (:pos ship)
                     ;        "Final point" point
                     ;        "Angle" angle
                     ;        "and last location" (last midturn-locations)
                     ;        "all locations" midturn-locations)]
                     ; _ (when (and (empty? (new-entities-between ship point (vals final-ships-to-compare)
                     ;                                            planet-compare-point))
                     ;              (seq (new-entities-between ship point (vals midturn-ships)
                     ;                                         planet-compare-point))
                     ;              (empty? (midturn-collisions midturn-locations
                     ;                                          (vals midturn-ships))))
                     ;     (log "WARNING: Collision for ship" (pretty-log ship)
                     ;          (pretty-log (new-entities-between ship point (vals midturn-ships)
                     ;                                            planet-compare-point))))]

                     ; midturn-locations (custom-math/calculate-end-positions
                     ;                    {:ship ship :thrust (min max-thrust thrust)
                     ;                     :angle angle})]
                 ; (log "Midturn locations" midturn-locations)
                 ; (log "Midturn collisions" (midturn-collisions midturn-locations
                 ;                                               (vals midturn-ships)))

                 (if (or (not (valid-point? point))
                         (and avoid-obstacles
                              (or (first (new-entities-between ship point (vals final-ships-to-compare)
                                                               planet-compare-point))
                                  ; (first (new-entities-between ship point (vals midturn-ships)
                                  ;                              planet-compare-point))
                                  (first (midturn-collisions midturn-locations
                                                             (vals midturn-ships)))))
                         ; (and avoid-attack (not (unreachable? point other-ships)))
                         (and avoid-attack (not (good-spot? point my-ships other-ships)))
                         (and avoid-corner (too-close-to-corner? point)))
                   (recur (rest iterations))
                   (do
                    ; (when (seq (new-entities-between ship point (vals midturn-ships)
                    ;                                  planet-compare-point))
                    ;   (log "WARNING: Collision for ship" (pretty-log ship)
                    ;        (pretty-log (new-entities-between ship point (vals midturn-ships)
                    ;                                          planet-compare-point))))
                    (log "I decided position: " point "was a good spot for ship " (:id ship))
                    (log "The enemies I could potentially look for included:" other-ships)
                    (assoc (e/thrust-move ship max-thrust angle) :subtype subtype))))))))))))

(defn navigate-to-fast
  "Returns a thrust move that moves the ship to the provided goal. The
  goal is treated as a point, i.e. the thrust move attempts to move
  the ship to the center of the goal. Use navigate-to-dock to compute
  a thrust move that does not collide with entities, or use
  closest-point yourself to find a suitable point. This function
  returns nil if it cannot find a suitable path."
  ([ship goal]
   (navigate-to-fast ship goal default-navigation-opts))
  ([ship goal {:keys [max-corrections avoid-obstacles
                      angular-step max-thrust buffer subtype avoid-attack avoid-corner]
               :as opts}]
   (let [
         buffer 0
         distance (math/distance-between ship goal)
         first-angle (math/orient-towards ship goal)
         compare-ships (remove (fn [[k v]]
                                  (or (= (:id ship) (:id v))))
                                      ; (= (:id goal) k)))
                                      ; (not= *player-id* (:owner-id v))))
                               *ships*)
         obstacles (figure-out-potential-obstacles ship (vals compare-ships))
         ; obstacles (figure-out-potential-obstacles ship (remove #(or (= (:id ship) (:id %))
         ;                                                             (= (:id goal) (:id %)))
         ;                                                        (vals *ships*)))
         my-ships (vals (filter (fn [[k v]]
                                  (and (integer? k)
                                       (= *player-id* (:owner-id v))))
                                compare-ships))
         other-ships (figure-out-potential-attacks ship (vals compare-ships))
         ; other-ships (when avoid-attack
         ;               (remove #(or (= *player-id* (:owner-id %))
         ;                            (not= :undocked (-> % :docking :status)))
         ;                       obstacles))
         thrust (int (min (- distance buffer) max-thrust))
         first-spot (custom-math/get-point ship thrust first-angle)
         good-spot (good-spot? first-spot my-ships other-ships)
         guaranteed-safe (when-not good-spot
                           (not (not-guaranteed-safe? ship other-ships)))
         thrust (if (not good-spot)
                  7
                  thrust)
         first-angle (if (and (not good-spot) (not guaranteed-safe))
                       (or (get-angle-to-run ship other-ships) first-angle)
                       first-angle)
         avoid-attack (if good-spot
                         false
                         guaranteed-safe)
         navigation-iterations (if (< thrust 7)
                                 (filter #(<= (:max-thrust %) thrust)
                                         all-navigation-iterations)
                                 all-navigation-iterations)]
     (if (and (not= :friendly2) (not good-spot))
       (navigate-to-friendly-ship-later ship)
       (if (< thrust buffer)
         (assoc (e/thrust-move ship 0 first-angle) :subtype subtype :reason "Fast - within buffer.")
         (loop [iterations (rest navigation-iterations)]
           (let [
                 ;; Timeout protection
                 times-up? (> (- (System/currentTimeMillis) *start-ms*) 1850)
                 {:keys [max-thrust angular-step]} (first iterations)]
             (if (or times-up? (nil? max-thrust))
               (assoc (e/thrust-move ship 0 first-angle) :subtype subtype :reason "Fast - ran out of possibilities.")
               (let [angle (+ first-angle angular-step)
                     point (custom-math/get-point ship max-thrust angle)
                     planet-compare-point (custom-math/get-point ship (min max-thrust planet-compare-distance) angle)]
                 (if (or (not (valid-point? point))
                         (and avoid-obstacles (first (new-entities-between ship point obstacles
                                                                           planet-compare-point)))
                         ; (and avoid-attack (not (unreachable? point other-ships)))
                         (and avoid-attack (not (good-spot? point my-ships other-ships)))
                         (and avoid-corner (too-close-to-corner? point)))
                   (recur (rest iterations))
                   (assoc (e/thrust-move ship max-thrust angle) :subtype subtype)))))))))))

(defn navigate-to
  "Determines which navigation to perform."
  ([ship goal]
   (navigate-to ship goal default-navigation-opts))
  ([ship goal {:keys [max-corrections avoid-obstacles
                      angular-step max-thrust buffer subtype avoid-attack]
               :as opts}]
   ; (if (> *num-ships* 1)
   (if (> *num-ships* 50)
     (navigate-to-fast ship goal opts)
     (navigate-to-precise ship goal opts))))


; (defn navigate-swarm-to
;   "Returns a thrust move that moves the ship to the provided goal. The
;   goal is treated as a point, i.e. the thrust move attempts to move
;   the ship to the center of the goal. Use navigate-to-dock to compute
;   a thrust move that does not collide with entities, or use
;   closest-point yourself to find a suitable point. This function
;   returns nil if it cannot find a suitable path."
;   ([ship goal]
;    (navigate-to ship goal default-navigation-opts))
;   ([ship goal {:keys [max-corrections avoid-obstacles
;                       angular-step max-thrust buffer subtype avoid-attack]
;                :as opts}]
;    (let [distance (math/distance-between ship goal)
;          first-angle (math/orient-towards ship goal)
;          ids (map :id (:swarm ship))
;          obstacles (figure-out-potential-obstacles ship (remove #(or (some (set [(:id %)]) ids)
;                                                                      (= (:id goal) (:id %)))
;                                                                 (vals *ships*)))
;          other-ships (when avoid-attack
;                        (remove #(or (= *player-id* (:owner-id %))
;                                     (not= :undocked (-> % :docking :status))))
;                                obstacles))
;          avoid-attack (if (and avoid-attack (unreachable? ship other-ships))
;                         avoid-attack
;                         false)
;          thrust (int (min (- distance buffer) max-thrust))]
;      ; (log "My ship is " ship)
;      ; (log "My goal is " goal)
;      ; (log "My distance to go is " distance)
;      (if (< distance buffer)
;        (for [single-ship (:swarm ship)]
;          (assoc (e/thrust-move single-ship 0 first-angle)
;                 :subtype subtype))
;        (loop [iterations (rest all-navigation-iterations)]
;          (let [{:keys [max-thrust angular-step]} (first iterations)]
;            (if (nil? max-thrust)
;              (for [single-ship (:swarm ship)]
;                (assoc (e/thrust-move single-ship 0 first-angle)
;                       :subtype subtype))
;              (let [angle (+ first-angle angular-step)
;                    point (custom-math/get-point ship (min max-thrust thrust) angle)]
;                (if (or (not (valid-point? point))
;                        (and avoid-attack (not (unreachable? point other-ships)))
;                        (and avoid-obstacles (first (swarm-entities-between ship point obstacles))))
;                  (recur (rest iterations))
;                  ;; One move for each ship in the swarm
;                  (for [single-ship (:swarm ship)
;                        :let [single-angle (math/orient-towards single-ship point)
;                              single-thrust (min max-thrust (int (math/distance-between single-ship
;                                                                                        point)))]]
;                    (assoc (e/thrust-move single-ship (min max-thrust single-thrust) single-angle)
;                           :subtype subtype)))))))))))

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
                (merge default-navigation-opts {
                                                ; :buffer 4.9
                                                :buffer 3.5
                                                ; :buffer 3.5
                                                :subtype :attack
                                                :avoid-attack avoid-attack?}))))
                                                ; :avoid-attack false}))))

; (defn navigate-swarm-to-attack-ship
;   "Navigate to with a buffer to not crash into ship."
;   [ship goal]
;   (navigate-swarm-to ship goal
;                      (merge default-navigation-opts {:buffer 3.5
;                                                      :subtype :swarm-attack
;                                                      :avoid-attack false})))

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
(def retreat-angular-step (/ Math/PI retreat-iterations))

(defn navigate-to-retreat-ship
  "Attempt to retreat and pull the ships away from my planets. Pick four points and make sure
  I cannot be attacked from them."
  [ship goal]
  (let [ships (remove #(or (= *player-id* (:owner-id %))
                           (not= :undocked (-> % :docking :status)))
                      (vals *ships*))
        not-safe (not-guaranteed-safe? ship ships)]
    (loop [angle (+ (* 33 (/ Math/PI 180.0)) (custom-math/orient-away ship goal))
           iteration 0]
      (let [goal (custom-math/get-point ship e/max-ship-speed angle)]
        (if (and (or not-safe (unreachable? goal ships))
                 (not (too-close-to-corner? goal))
                 (valid-point? goal))
          (navigate-to ship goal (merge default-navigation-opts {:buffer 0.0
                                                                 :max-thrust e/max-ship-speed
                                                                 :subtype :retreat-ship}))
          (if (<= iteration retreat-iterations)
            (recur (+ retreat-angular-step angle)
                   (inc iteration))))))))

; (defn navigate-swarm-to-retreat-ship
;   "Attempt to retreat and pull the ships away from my planets. Pick four points and make sure
;   I cannot be attacked from them."
;   [ship goal]
;   (let [ships (remove #(or (= *player-id* (:owner-id %))
;                            (not= :undocked (-> % :docking :status)))
;                       (vals *ships*))
;         not-safe (not-guaranteed-safe? ship ships)]
;     (loop [angle (custom-math/orient-away ship goal)
;            iteration 0]
;       (let [goal (custom-math/get-point ship e/max-ship-speed angle)]
;         (if (or not-safe (unreachable? goal ships))
;           (navigate-swarm-to ship goal (merge default-navigation-opts {:buffer 1.01
;                                                                        :max-thrust e/max-ship-speed
;                                                                        :subtype :swarm-retreat}))
;           (if (<= iteration retreat-iterations)
;             (recur (mod (+ retreat-angular-step angle) 360)
;                    (inc iteration))))))))


(defn navigate-to-swarm-ship
  "Returns a thrust move which will navigate this ship to the requested
  planet for docking. The ship will attempt to get to
  `docking-distance` units above the planet's surface. Returns nil if
  it cannot find a suitable path."
  [ship friendly-ship]
  (navigate-to ship friendly-ship (merge default-navigation-opts {:buffer 1.01
                                                                  :subtype :swarm-friendly})))
                                                                  ; :max-corrections 10})))

(defn navigate-to-friendly-ship
  "Returns a thrust move which will navigate this ship to the requested
  planet for docking. The ship will attempt to get to
  `docking-distance` units above the planet's surface. Returns nil if
  it cannot find a suitable path."
  [ship friendly-ship]
  (navigate-to ship friendly-ship (merge default-navigation-opts {:buffer 1.01
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
                   ; (/ (* 2 distance) 3
                   (max 2 (- distance 5))
                   2)
                   ; (min 3 (- distance 3)))
                   ; (/ (* 1 distance) 10)
        midpoint (custom-math/get-point friendly-ship distance angle)]
    ; (if (and (not advantage?) (< distance 7))
    ;   (navigate-to ship (custom-math/get-point friendly-ship 7 angle)
    ;                (merge default-navigation-opts {:buffer 0.0 :subtype :suicide}))
    ; (if advantage?
    ;   (navigate-to-attack-ship ship enemy-ship)
    (navigate-to ship midpoint (merge default-navigation-opts {:buffer 0.0
                                                               :subtype :defend
                                                               :avoid-attack true}))))

(defn navigate-to-dock
  "Returns a thrust move which will navigate this ship to the requested
  planet for docking. The ship will attempt to get to
  `docking-distance` units above the planet's surface. Returns nil if
  it cannot find a suitable path."
  [ship planet docking-point]
    ; (log "Trying to navigate to docking point" docking-point "for ship " (:id ship))
    ; (log "Distance to docking point" (math/distance-between ship docking-point))
  (navigate-to ship docking-point (assoc default-navigation-opts :subtype :dock)))
    ; (navigate-to-fast ship docking-point (assoc default-navigation-opts :subtype :dock))))

(def too-close-distance 1.0)

(defn too-close-to-planet
  "Returns true if the point is < than 3 units away from a planet."
  [position planet]
  (let [distance-from-planet (- (math/distance-between position planet) (:radius planet))]
    (< distance-from-planet too-close-distance)))

(defn navigate-to-retreat
  "Attempt to retreat and pull the ships away from my planets and towards their own.
  I cannot be attacked from them."
  ([ship planet]
   (navigate-to-retreat ship planet true))
  ([ship planet avoid-corner]
   ; (log "Navigate to retreat")
   (navigate-to ship planet (merge default-navigation-opts {:buffer 0.0
                                                            :max-thrust e/max-ship-speed
                                                            ; :subtype :retreat3
                                                            :subtype :friendly2
                                                            :avoid-corner avoid-corner}))))

; (defn navigate-to-retreat
;   "Attempt to retreat and pull the ships away from my planets and towards their own.
;   I cannot be attacked from them."
;   ([ship planet]
;    (navigate-to-retreat ship planet true))
;   ([ship planet avoid-corner]
;    (log "Navigate to retreat")
;    (let [ships (remove #(or (= *player-id* (:owner-id %))
;                             (not= :undocked (-> % :docking :status)))
;                        (vals *ships*))
;          orig-planet planet
;          not-safe (not-guaranteed-safe? ship ships)
;          orig-angle (if not-safe
;                       (custom-math/orient-away ship (nearest-entity ship ships))
;                       (math/orient-towards ship planet))]
;      (loop [angle orig-angle
;             iteration 0]
;        (let [planet (custom-math/get-point ship e/max-ship-speed angle)]
;          (if (and (or not-safe (unreachable? planet ships))
;                   (not (too-close-to-planet planet orig-planet))
;                   (not (too-close-to-corner? planet))
;                   (valid-point? planet))
;            (navigate-to ship planet (merge default-navigation-opts {:buffer 0.0
;                                                                     :max-thrust e/max-ship-speed
;                                                                     :subtype :retreat3
;                                                                     :avoid-corner avoid-corner}))
;            (when (<= iteration retreat-iterations)
;              (recur (+ retreat-angular-step angle)
;                     (inc iteration)))))))))
           ; (when (<= iteration retreat-iterations)
           ;   (let [angle (if (even? iteration)
           ;                 (mod (+ (* retreat-angular-step (int (/ iteration 2))) orig-angle) 360)
           ;                 (mod (+ (* retreat-angular-step (int (/ (inc iteration) 2)) -1) orig-angle) 360))]
           ;     (recur angle
           ;            (inc iteration))))))))))

; (defn navigate-swarm-to-retreat
;   "Attempt to retreat and pull the ships away from my planets and towards their own.
;   I cannot be attacked from them."
;   [ship planet]
;   (let [ships (remove #(or (= *player-id* (:owner-id %))
;                            (not= :undocked (-> % :docking :status)))
;                       (vals *ships*))
;         orig-planet planet
;         not-safe (not-guaranteed-safe? ship ships)
;         orig-angle (if not-safe
;                      (custom-math/orient-away ship (nearest-entity ship ships))
;                      (math/orient-towards ship planet))]
;     (loop [angle orig-angle
;            iteration 0]
;       (let [planet (custom-math/get-point ship e/max-ship-speed angle)]
;         (if (and (or not-safe (unreachable? planet ships))
;                  (not (too-close-to-planet planet orig-planet)))
;           (navigate-swarm-to ship planet (merge default-navigation-opts {:buffer 0.0
;                                                                          :max-thrust e/max-ship-speed
;                                                                          :subtype :swarm-retreat3}))
;           (when (<= iteration retreat-iterations)
;             (let [angle (if (even? iteration)
;                           (mod (+ (* retreat-angular-step (int (/ iteration 2))) orig-angle) 360)
;                           (mod (+ (* retreat-angular-step (int (/ (inc iteration) 2)) -1) orig-angle) 360))]
;               (recur angle
;                      (inc iteration)))))))))

(defn navigate-to-nearest-corner
  "Navigate to the nearest corner for this stupid strategy everyone is doing."
  [position]
  (let [[max-x max-y] *map-size*
        ne (math/->Position (dec max-x) 0.1)
        nw (math/->Position 0.1 0.1)
        se (math/->Position (dec max-x) (dec max-y))
        sw (math/->Position 0.1 (dec max-y))]
    (navigate-to-retreat position (assoc (closest-position position [ne nw se sw]) :radius 0.0)
                         false)))
