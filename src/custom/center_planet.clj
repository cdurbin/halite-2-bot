(ns custom.center-planet
  "Functions specific to dealing with the center planets. Broken out from map analysis for now."
  (:require
   [custom.game-map :refer [*num-players* *num-ships*]]
   [custom.utils :as utils]
   [custom.math :refer [infinity]]
   [hlt.entity :as e]
   [hlt.game-map :refer [*map-size* *planets*]]
   [hlt.math :as math]
   [hlt.navigation :as hlt-navigation]
   [hlt.utils :refer [log]]))

(def center-planets (atom nil))

(defn center-planet?
  "Returns true if the planet is a center planet."
  [planet]
  (some #{(:id planet)} @center-planets))

(defn get-center-point
  "Returns the center point on the map."
  []
  (let [[max-x max-y] *map-size*]
    (math/->Position (/ max-x 2) (/ max-y 2))))

(defn find-four-center-planets
  "Returns the four-center-planets"
  []
  (let [midpoint (get-center-point)
        planet-distances (for [planet (vals *planets*)
                               :let [distance (math/distance-between planet midpoint)]]
                           {:planet planet
                            :distance distance})]
    (map :planet (take 4 (sort (utils/compare-by :distance utils/asc) planet-distances)))))

(defn get-best-docking-point
  "Returns the best point ot dock on a planet. The best place is to dock where the ships spawn,
  which is the point closest to the center of the map."
  [ship planet]
  (if (center-planet? planet)
    (let [
          ; _ (log "Yes this is a center planet" (:id planet))
          center (get-center-point)]
      (math/closest-point center planet 0))
    (math/closest-point ship planet 0)))

(defn closest-to-edge
  "Returns the planet that is closest to the corner in a list of planets."
  [planets]
  (let [midpoint (get-center-point)
        planet-distances (for [planet (vals *planets*)
                               :let [distance (math/distance-between planet midpoint)]]
                           {:planet planet
                            :distance distance})]
    (-> (sort (utils/compare-by :distance utils/desc) planet-distances)
        first
        :planet)))

(defn num-turns-to-planet
  "Returns the number of turns to get to a docking spot on a planet from a given point."
  [start planet]
  (inc (int (/ (- (math/distance-between start planet) (:radius planet) 0.01)
               e/max-ship-speed))))

(defn num-turns-to-new-ship
  "How many turns before a new ship will dock."
  [planet]
  nil)

(defn get-turns-to-new-ship-planet
  "Returns how many turns before a new ship will spawn for this docked ship's planet."
  [planet]
  (let [planet-progress (-> planet :docking :current-production)
        num-docked-ships (-> planet :docking :ships count)
        remaining-to-ship (- 72 planet-progress)]
    (int (/ remaining-to-ship (* num-docked-ships 6)))))

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

(defn add-priority-to-planets
  "Planets get priority based on their distance from the center. In four player games the
  priority is the outside and 2 player games the priority is the center."
  [planets]
  (log "APTP: Num ships is: " *num-ships*)
  (let [midpoint (get-center-point)
        neutral-planets (filter #(nil? (:owner-id %))
                                (vals *planets*))
        bonus-for-3-spots (if (= *num-ships* 3)
                            42 ;; 6 turns - really 3 turns because we double the distance in 2 players
                            0)]
    (for [planet (vals *planets*)
          :let [distance (if (= 2 *num-players*)
                           0
                           (math/distance-between planet midpoint))
                ; dock-spot-value (* 7 (get-in planet [:docking :spots]))
                dock-spot-value (* 14 (get-in planet [:docking :spots]))
                next-neutral-planet (nearest-entity planet neutral-planets)
                distance-to-next-neutral-planet (if next-neutral-planet
                                                  (- (math/distance-between planet next-neutral-planet)
                                                     (:radius planet) (:radius next-neutral-planet))
                                                  0)
                distance-to-next-neutral-planet (* -1 distance-to-next-neutral-planet)
                bonus (if (>= (get-in planet [:docking :spots]) 3)
                        bonus-for-3-spots
                        0)
                priority (if (= *num-players* 2)
                            (- distance dock-spot-value distance-to-next-neutral-planet bonus)
                            (+ distance dock-spot-value distance-to-next-neutral-planet bonus))]]
      (assoc planet :priority priority))))
    ;   {:planet planet
    ;    :distance (if (= *num-players* 2)
    ;                (- distance dock-spot-value distance-to-next-neutral-planet)
    ;                (+ distance dock-spot-value distance-to-next-neutral-planet))
    ;     ; direction (if (= *num-players* 2)
    ;     ;             utils/asc
    ;     ;             utils/desc)
    ;     _ (log "PS: direction is" direction)})
    ; (map #(assoc (:planet %) :priority (:distance %)))))
    ;     planets-in-order (sort (utils/compare-by :distance direction) planet-distances))
    ; (map-indexed (fn [idx planet]
    ;                (assoc planet :priority (inc idx)))
    ;              (map :planet planets-in-order))))

(defn get-turns-to-planet
  "Returns a collection of maps with keys of :planet and :turns. Takes into account the radius
  of the planet. Returned in sorted order from fewest to most."
  [point planets]
  (let [planet-turns (for [planet planets
                           :let [turns (num-turns-to-planet point planet)]]
                       {:planet planet
                        :turns turns})]
    (sort (utils/compare-by :turns utils/asc) planet-turns)))

(defn rescore-planet
  "Rescores the value of the planet based on the provided point."
  [pos planet]
  (let [distance (- (math/distance-between pos planet) (:radius planet))
        priority (if (= *num-players* 2)
                   (+ (:priority planet) (* 2 distance))
                   (- (:priority planet) distance))]
    (assoc planet :priority priority)))
