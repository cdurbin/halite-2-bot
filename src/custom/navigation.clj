(ns custom.navigation
  (:require
   [custom.math :as custom-math]
   [hlt.entity :as e]
   [hlt.navigation :as hlt-navigation]
   [hlt.game-map :refer [*planets* *ships*]]
   [hlt.math :as math :refer [get-x get-y]]))

(def default-navigation-opts
  (assoc hlt-navigation/default-navigation-opts :max-corrections 220))
  ; (assoc hlt-navigation/default-navigation-opts :max-corrections 180))

(def reverse-nagivation-opts
  (assoc default-navigation-opts :angular-step (/ Math/PI -180.0)))

; (def max-distance-visibility
;   "How far to look for no obstacles."
;   20)

(defn figure-out-potential-obstacles
  "TODO: Note planets I should look further out, but ships this is fine."
  [ship goal all-ships]
  (filter #(< (math/distance-between ship %) (+ e/max-ship-speed 0.5)) all-ships))

  ; (let [to-goal-angle (math/orient-towards ship goal)
  ;       minus-90-angle (math/rad->deg (- to-goal-angle (/ Math/PI 2)))
  ;       plus-90-angle (math/rad->deg (+ to-goal-angle (/ Math/PI 2)))]
  ;   (filter #(and (< (math/distance-between ship %) e/max-ship-speed)
  ;                 (custom-math/between plus-90-angle minus-90-angle
  ;                                      (math/rad->deg (math/orient-towards ship %))))
  ;           all-ships)))

(defn new-entities-between
  "More efficient entities-between"
  [a b obstacles]
  (let [filter-fn #(and (distinct? a b %)
                        (math/segment-circle-intersects? a b % hlt-navigation/default-fudge-factor))]
    (concat (filter filter-fn (vals *planets*))
            (filter filter-fn obstacles))))

(defn navigate-to-attack-ship
  "Returns a thrust move that moves the ship to the provided goal. The
  goal is treated as a point, i.e. the thrust move attempts to move
  the ship to the center of the goal. Use navigate-to-dock to compute
  a thrust move that does not collide with entities, or use
  closest-point yourself to find a suitable point. This function
  returns nil if it cannot find a suitable path."
  ([ship goal]
   (navigate-to-attack-ship ship goal default-navigation-opts))
  ([ship goal {:keys [max-corrections avoid-obstacles
                      angular-step max-thrust]
               :as opts}]
   (let [distance (math/distance-between ship goal)
         first-angle (math/orient-towards ship goal)
        ;  goal (if (> distance max-distance-visibility)
        ;         (custom-math/get-point ship max-distance-visibility first-angle)
        ;         goal)
         obstacles (figure-out-potential-obstacles ship goal (vals *ships*))]
     (if (< distance 5)
       (e/thrust-move ship 0 first-angle)
       (loop [goal goal
              iteration 0]
         (if (<= max-corrections iteration)
           (e/thrust-move ship 0 first-angle)
          ;  (let [distance (math/distance-between ship goal)]
           (let [angular-step (* -1 angular-step)
                 angle (math/orient-towards ship goal)]
             (if (and avoid-obstacles (first (new-entities-between ship goal obstacles)))
               (let [new-target-dx (* (Math/cos (+ first-angle (* 0.5 iteration angular-step))) distance)
                     new-target-dy (* (Math/sin (+ first-angle (* 0.5 iteration angular-step))) distance)
                     new-goal (math/->Position (+ new-target-dx (get-x ship))
                                               (+ new-target-dy (get-y ship)))]
                 (recur new-goal (inc iteration)))
              ;  (let [thrust (int (min (- distance 1.5) max-thrust))])
               (let [thrust (int (min (max 0 (- distance 4.1)) max-thrust))]
                 (e/thrust-move ship thrust angle))))))))))

(defn navigate-to-attack-docked-ship
  "Returns a thrust move that moves the ship to the provided goal. The
  goal is treated as a point, i.e. the thrust move attempts to move
  the ship to the center of the goal. Use navigate-to-dock to compute
  a thrust move that does not collide with entities, or use
  closest-point yourself to find a suitable point. This function
  returns nil if it cannot find a suitable path."
  ([ship goal]
   (navigate-to-attack-ship ship goal default-navigation-opts))
  ([ship goal {:keys [max-corrections avoid-obstacles
                      angular-step max-thrust]
               :as opts}]
   (let [distance (math/distance-between ship goal)
         first-angle (math/orient-towards ship goal)
        ;  goal (if (> distance max-distance-visibility)
        ;         (custom-math/get-point ship max-distance-visibility first-angle)
        ;         goal)
         obstacles (figure-out-potential-obstacles ship goal (vals *ships*))]
     (if (< distance 5)
       (e/thrust-move ship 0 first-angle)
       (loop [goal goal
              iteration 0]
         (if (<= max-corrections iteration)
           (e/thrust-move ship 0 first-angle)
           (let [angular-step (* -1 angular-step)
                 angle (math/orient-towards ship goal)]
             (if (and avoid-obstacles (first (new-entities-between ship goal obstacles)))
               (let [new-target-dx (* (Math/cos (+ first-angle (* 0.5 iteration angular-step))) distance)
                     new-target-dy (* (Math/sin (+ first-angle (* 0.5 iteration angular-step))) distance)
                     new-goal (math/->Position (+ new-target-dx (get-x ship))
                                               (+ new-target-dy (get-y ship)))]
                 (recur new-goal (inc iteration)))
               (let [thrust (int (min (- distance 1.5) max-thrust))]
              ;  (let [thrust (int (min (max 0 (- distance 7.1)) max-thrust))]
                 (e/thrust-move ship thrust angle))))))))))

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
                      angular-step max-thrust]
               :as opts}]
   (let [distance (math/distance-between ship goal)
         first-angle (math/orient-towards ship goal)
        ;  goal (if (> distance max-distance-visibility)
        ;         (custom-math/get-point ship max-distance-visibility first-angle)
        ;         goal)
         obstacles (figure-out-potential-obstacles ship goal (vals *ships*))]
     (loop [goal goal
            iteration 0]
       (if (<= max-corrections iteration)
         (e/thrust-move ship 0 first-angle)
         (let [angular-step (* -1 angular-step)
               distance (math/distance-between ship goal)
               angle (math/orient-towards ship goal)]
           (if (and avoid-obstacles (first (new-entities-between ship goal obstacles)))
             (let [new-target-dx (* (Math/cos (+ first-angle (* 0.5 iteration angular-step))) distance)
                   new-target-dy (* (Math/sin (+ first-angle (* 0.5 iteration angular-step))) distance)
                   new-goal (math/->Position (+ new-target-dx (get-x ship))
                                             (+ new-target-dy (get-y ship)))]
               (recur new-goal (inc iteration)))
             (let [thrust (int (min distance max-thrust))]
               (e/thrust-move ship thrust angle)))))))))

(defn navigate-to-retreat
  "Attempt to retreat"
  ([ship goal]
   (navigate-to-retreat ship goal default-navigation-opts))
  ([ship goal {:keys [max-corrections avoid-obstacles
                      angular-step max-thrust]}]
   (let [distance (math/distance-between ship goal)
         first-angle (custom-math/orient-away ship goal)
         goal (custom-math/get-point ship max-thrust first-angle)
         obstacles (figure-out-potential-obstacles ship goal (vals *ships*))]
     (loop [goal goal
            iteration 0]
       (if (<= max-corrections iteration)
         (e/thrust-move ship 0 first-angle)
         (let [angular-step (* -1 angular-step)
               angle (math/orient-towards ship goal)]
           (if (and avoid-obstacles (first (new-entities-between ship goal obstacles)))
             (let [new-target-dx (* (Math/cos (+ first-angle (* 0.5 iteration angular-step))) distance)
                   new-target-dy (* (Math/sin (+ first-angle (* 0.5 iteration angular-step))) distance)
                   new-goal (math/->Position (+ new-target-dx (get-x ship))
                                             (+ new-target-dy (get-y ship)))]
               (recur new-goal (inc iteration)))
             (let [thrust (if (> distance 10)
                            (int (/ max-thrust 4))
                            max-thrust)]
               (e/thrust-move ship thrust angle)))))))))

(defn navigate-to-dock
  "Returns a thrust move which will navigate this ship to the requested
  planet for docking. The ship will attempt to get to
  `docking-distance` units above the planet's surface. Returns nil if
  it cannot find a suitable path."
  ([ship planet]
   (navigate-to-dock ship planet {}))
  ([ship planet opts]
   (let [docking-point (math/closest-point ship planet hlt-navigation/docking-distance)]
     (navigate-to ship docking-point
                  (merge default-navigation-opts opts)))))
