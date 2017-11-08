(ns custom.navigation
  (:require
   [custom.math :as custom-math]
   [hlt.entity :as e]
   [hlt.navigation :as hlt-navigation]
   [hlt.math :as math :refer [get-x get-y]]))

(def navigate-to-dock hlt-navigation/navigate-to-dock)

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
   (let [orig-distance (math/distance-between ship goal)
         potential-enemy-thrust (if (= :undocked (-> goal :docking :status))
                                  max-thrust
                                  0)]
     (loop [goal goal
            opts opts]
       (if (<= (:max-corrections opts) 0)
         nil
         (let [distance (math/distance-between ship goal)]
           (if (< distance 5)
             nil
             (let [angle (math/orient-towards ship goal)]
               (if (and avoid-obstacles (first (hlt-navigation/entities-between ship goal)))
                 (let [new-target-dx (* (Math/cos (+ angle angular-step)) distance)
                       new-target-dy (* (Math/sin (+ angle angular-step)) distance)
                       new-goal (math/->Position (+ new-target-dx (get-x ship))
                                                 (+ new-target-dy (get-y ship)))]
                   (recur new-goal (update opts :max-corrections dec)))
                 (let [thrust (int (min (- distance 1.5) max-thrust))]
                ;  (let [thrust (int (min (max 0 (- orig-distance potential-enemy-thrust))
                ;                         max-thrust))]
                   (e/thrust-move ship thrust angle)))))))))))

(defn navigate-to-retreat
  "Returns a thrust move that moves the ship to the provided goal. The
  goal is treated as a point, i.e. the thrust move attempts to move
  the ship to the center of the goal. Use navigate-to-dock to compute
  a thrust move that does not collide with entities, or use
  closest-point yourself to find a suitable point. This function
  returns nil if it cannot find a suitable path."
  ([ship goal]
   (navigate-to-retreat ship goal hlt-navigation/default-navigation-opts))
  ([ship goal {:keys [max-corrections avoid-obstacles
                      angular-step max-thrust]
               :as opts}]
   (let [orig-distance (math/distance-between ship goal)
         angle (custom-math/orient-away ship goal)
         goal (custom-math/get-point ship max-thrust angle)]
     (loop [goal goal
            opts opts]
       (if (<= (:max-corrections opts) 0)
         nil
         (let [distance (math/distance-between ship goal)
               angle (math/orient-towards ship goal)]
           (if (and avoid-obstacles (first (hlt-navigation/entities-between ship goal)))
             (let [new-target-dx (* (Math/cos (+ angle angular-step)) distance)
                   new-target-dy (* (Math/sin (+ angle angular-step)) distance)
                   new-goal (math/->Position (+ new-target-dx (get-x ship))
                                             (+ new-target-dy (get-y ship)))]
               (recur new-goal (update opts :max-corrections dec)))

             (let [thrust (if (> orig-distance 10)
                            (int (/ max-thrust 4))
                            max-thrust)]
               (e/thrust-move ship thrust angle)))))))))
