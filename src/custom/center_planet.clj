(ns custom.center-planet
  "Functions specific to dealing with the center planets. Broken out from map analysis for now."
  (:require
   [custom.game-map :refer [*num-players*]]
   [custom.utils :as utils]
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

(defn add-priority-to-planets
  "Planets get priority based on their distance from the center. In four player games the
  priority is the outside and 2 player games the priority is the center."
  [planets]
  (let [midpoint (get-center-point)
        planet-distances (for [planet (vals *planets*)
                               :let [distance (math/distance-between planet midpoint)]]
                           {:planet planet
                            :distance distance})
        direction (if (= *num-players* 2)
                    utils/asc
                    utils/desc)
        planets-in-order (sort (utils/compare-by :distance utils/desc) planet-distances)]
    (map-indexed (fn [idx planet]
                   (assoc planet :priority (inc idx)))
                 (map :planet planets-in-order))))

(defn get-turns-to-planet
  "Returns a collection of maps with keys of :planet and :turns. Takes into account the radius
  of the planet. Returned in sorted order from fewest to most."
  [point planets]
  (let [planet-turns (for [planet planets
                           :let [turns (num-turns-to-planet point planet)]]
                       {:planet planet
                        :turns turns})]
    (sort (utils/compare-by :turns utils/asc) planet-turns)))
