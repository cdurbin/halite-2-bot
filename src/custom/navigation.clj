(ns custom.navigation
  (:require
   [custom.math :as custom-math]
   [hlt.entity :as e]
   [hlt.utils :refer [log]]
   [hlt.navigation :as hlt-navigation]
   [hlt.game-map :refer [*planets* *ships* *player-id* *map-size*]]
   [hlt.math :as math :refer [get-x get-y]]))

(def default-navigation-opts
  (assoc hlt-navigation/default-navigation-opts
         :max-corrections 200 :buffer 0))

(def reverse-nagivation-opts
  (assoc default-navigation-opts :angular-step (/ Math/PI -180.0)))

(defn valid-point?
  "Returns true if the point is on the map."
  [point]
  (let [[max-x max-y] *map-size*
        x (get-x point)
        y (get-y point)]
    (and (< 0 x max-x)
         (< 0 y max-y))))

(defn figure-out-potential-obstacles
  "TODO: Note planets I should look further out, but ships this is fine."
  [ship goal all-ships]
  (filter #(< (math/distance-between ship %) (+ e/max-ship-speed 2.2)) all-ships))

(def slightly-smaller-fudge-factor 0.6)

(defn new-entities-between
  "More efficient entities-between"
  [a b obstacles]
  (let [filter-fn #(and (distinct? a b %)
                        (math/segment-circle-intersects? a b % slightly-smaller-fudge-factor))]
    (concat (filter filter-fn (vals *planets*))
            (filter filter-fn obstacles))))

(def all-navigation-iterations
  "Returns the angular-step and max thrust for each potential navigation iteration."
  (for [iterations (range 1 10)
        thrust [7 6 5 4]
        ; thrust [7 6 5 4 3 2 1]
        angular-step (range 10)
        opposite (range 2)
        :let [angular-step (* (/ Math/PI 180.0) angular-step)]]
    {:max-thrust thrust
     :angular-step (if (zero? opposite) (* -1 iterations angular-step) (* iterations angular-step))}))


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
                      angular-step max-thrust buffer subtype]
               :as opts}]
   (let [distance (math/distance-between ship goal)
         first-angle (math/orient-towards ship goal)
         obstacles (figure-out-potential-obstacles ship goal (vals *ships*))
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
                       (and avoid-obstacles (first (new-entities-between ship point obstacles))))
                   (recur (rest iterations))
                 (assoc (e/thrust-move ship (min max-thrust thrust) angle) :subtype subtype))))))))))

; (defn navigate-to
;   "Returns a thrust move that moves the ship to the provided goal. The
;   goal is treated as a point, i.e. the thrust move attempts to move
;   the ship to the center of the goal. Use navigate-to-dock to compute
;   a thrust move that does not collide with entities, or use
;   closest-point yourself to find a suitable point. This function
;   returns nil if it cannot find a suitable path."
;   ([ship goal]
;    (navigate-to ship goal default-navigation-opts))
;   ([ship goal {:keys [max-corrections avoid-obstacles
;                       angular-step max-thrust buffer subtype]
;                :as opts}]
;    (let [distance (math/distance-between ship goal)
;          first-angle (math/orient-towards ship goal)
;          obstacles (figure-out-potential-obstacles ship goal (vals *ships*))
;          thrust (int (min (- distance buffer) max-thrust))]
;      (if (< distance buffer)
;        (e/thrust-move ship 0 first-angle)
;        (loop [goal goal
;               iteration 0]
;          (if (<= max-corrections iteration)
;            (e/thrust-move ship 0 first-angle)
;            (let [angular-step (* -1 angular-step)
;                  angle (math/orient-towards ship goal)
;                  point (custom-math/get-point ship thrust angle)]
;              (if (or (not (valid-point? point))
;                      (and avoid-obstacles (first (new-entities-between ship goal obstacles))))
;                (let [new-target-dx (* (Math/cos (+ first-angle (* 0.5 iteration angular-step))) distance)
;                      new-target-dy (* (Math/sin (+ first-angle (* 0.5 iteration angular-step))) distance)
;                      new-goal (math/->Position (+ new-target-dx (get-x ship))
;                                                (+ new-target-dy (get-y ship)))]
;                  (recur new-goal (inc iteration)))
;                (assoc (e/thrust-move ship thrust angle) :subtype subtype)))))))))

(defn navigate-to-attack-ship
  "Navigate to with a buffer to not crash into ship."
  [ship goal]
  (navigate-to ship goal (merge default-navigation-opts {:buffer 4.5 :subtype :attack})))

(defn navigate-to-attack-docked-ship
  "Navigate to with a buffer to not crash into ship."
  [ship goal]
  (navigate-to ship goal (merge default-navigation-opts {:buffer 1.1 :subtype :docked-attack})))

(defn unreachable?
  "Returns true if none of the passed in ships can attack this position on the map."
  [position ships]
  (let [safe-radius (+ e/max-ship-speed (* 2 e/ship-radius) e/weapon-radius)]
    (not (some? (seq (filter #(< (math/distance-between position %) safe-radius) ships))))))

(def retreat-iterations 18)
(def retreat-angular-step (/ 360 retreat-iterations))

(defn navigate-to-retreat-ship
  "Attempt to retreat and pull the ships away from my planets. Pick four points and make sure
  I cannot be attacked from them."
  [ship goal]
  (let [ships (remove #(or (= *player-id* (:owner-id %))
                           (not= :undocked (-> % :docking :status)))
                      (vals *ships*))]
    (loop [angle (custom-math/orient-away ship goal)
           iteration 0]
      (let [goal (custom-math/get-point ship e/max-ship-speed angle)]
        (if (unreachable? goal ships)
          (navigate-to ship goal (merge default-navigation-opts {:buffer 0.0
                                                                 :max-thrust e/max-ship-speed
                                                                 :subtype :retreat}))
          (when (<= iteration retreat-iterations)
            (recur (mod (+ retreat-angular-step angle) 360)
                   (inc iteration))))))))

(defn navigate-to-friendly-ship
  "Returns a thrust move which will navigate this ship to the requested
  planet for docking. The ship will attempt to get to
  `docking-distance` units above the planet's surface. Returns nil if
  it cannot find a suitable path."
  [ship friendly-ship]
  (navigate-to ship friendly-ship (merge default-navigation-opts {:buffer 1.1
                                                                  :subtype :friendly})))

(defn navigate-to-dock
  "Returns a thrust move which will navigate this ship to the requested
  planet for docking. The ship will attempt to get to
  `docking-distance` units above the planet's surface. Returns nil if
  it cannot find a suitable path."
  [ship planet]
  (let [docking-point (math/closest-point ship planet hlt-navigation/docking-distance)]
    (navigate-to ship docking-point (assoc default-navigation-opts :subtype :dock))))

(defn navigate-to-retreat
  "Attempt to retreat and pull the ships away from my planets and towards their own.
  I cannot be attacked from them."
  [ship planet]
  (let [ships (remove #(or (= *player-id* (:owner-id %))
                           (not= :undocked (-> % :docking :status)))
                      (vals *ships*))]
    (loop [angle (math/orient-towards ship planet)
           iteration 0]
      (let [planet (custom-math/get-point ship e/max-ship-speed angle)]
        (if (unreachable? planet ships)
          (navigate-to ship planet (merge default-navigation-opts {:buffer 0.0
                                                                   :max-thrust e/max-ship-speed
                                                                   :subtype :retreat}))
          (when (<= iteration retreat-iterations)
            (recur (mod (+ retreat-angular-step angle) 360)
                   (inc iteration))))))))

(def infinity 99999999)

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
    (navigate-to position (closest-position position [ne nw se sw]))))
