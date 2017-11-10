(ns custom.navigation
  (:require
   [hlt.entity :as e]
   [hlt.navigation :as hlt-navigation]
   [hlt.math :as math :refer [get-x get-y]]))

(def default-navigation-opts
  (assoc hlt-navigation/default-navigation-opts :max-corrections 180))

(def reverse-nagivation-opts
  (assoc default-navigation-opts :angular-step (/ Math/PI -180.0)))

(defn navigate-to-attack-ship
  "Returns a thrust move that moves the ship to the provided goal. The
  goal is treated as a point, i.e. the thrust move attempts to move
  the ship to the center of the goal. Use navigate-to-dock to compute
  a thrust move that does not collide with entities, or use
  closest-point yourself to find a suitable point. This function
  returns nil if it cannot find a suitable path."
  ([ship goal]
   (navigate-to-attack-ship ship goal hlt-navigation/default-navigation-opts))
  ([ship goal {:keys [max-corrections avoid-obstacles
                      angular-step max-thrust]
               :as opts}]
   (let [distance (math/distance-between ship goal)
         first-angle (math/orient-towards ship goal)]
     (loop [goal goal
            iteration 0]
       (if (<= max-corrections iteration)
         nil
         (let [distance (math/distance-between ship goal)]
           (if (< distance 5)
             nil
             (let [angular-step (* -1 angular-step)
                   angle (math/orient-towards ship goal)]
               (if (and avoid-obstacles (first (hlt-navigation/entities-between ship goal)))
                 (let [new-target-dx (* (Math/cos (+ first-angle (* 0.5 iteration angular-step))) distance)
                       new-target-dy (* (Math/sin (+ first-angle (* 0.5 iteration angular-step))) distance)
                       new-goal (math/->Position (+ new-target-dx (get-x ship))
                                                 (+ new-target-dy (get-y ship)))]
                   (recur new-goal (inc iteration)))
                 (let [thrust (int (min (- distance 1.5) max-thrust))]
                   (e/thrust-move ship thrust angle)))))))))))

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
         first-angle (math/orient-towards ship goal)]
     (loop [goal goal
            iteration 0]
       (if (<= max-corrections iteration)
         nil
         (let [angular-step (* -1 angular-step)
               distance (math/distance-between ship goal)
               angle (math/orient-towards ship goal)]
           (if (and avoid-obstacles (first (hlt-navigation/entities-between ship goal)))
             (let [new-target-dx (* (Math/cos (+ first-angle (* 0.5 iteration angular-step))) distance)
                   new-target-dy (* (Math/sin (+ first-angle (* 0.5 iteration angular-step))) distance)
                   new-goal (math/->Position (+ new-target-dx (get-x ship))
                                             (+ new-target-dy (get-y ship)))]
               (recur new-goal (inc iteration)))
             (let [thrust (int (min distance max-thrust))]
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
