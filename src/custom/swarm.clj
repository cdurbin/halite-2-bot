(ns custom.swarm
  "Functions for making flying in fleets easier."
  (:require
   [custom.map-analysis :as map]
   [custom.math :as custom-math]
   [custom.navigation :as navigation]
   [custom.utils :as utils :refer [pretty-log]]
   [hlt.entity :as e]
   [hlt.game-map :refer [*ships*]]
   [hlt.math :as math]
   [hlt.utils :refer [log]]))


(def max-ships-in-swarm 3)
(def swarm-eligible-distance-apart 10)
(def how-close-to-swarm 1.1)
(def max-swarm-range 1.5)

(defn- get-ship-distances
  "Helper to return a collection of ships and max ship distances from that ship to the others."
  [ships]
  (for [ship ships
        :let [max-distance (math/distance-between ship (map/farthest-entity ship ships))]]
    {:ship ship :distance max-distance}))

(defn identify-middle-ship
  "Returns the middle ship from a swarm."
  [ships]
  (let [ship-distances (get-ship-distances ships)]
    (->> ship-distances
         (sort-by :distance)
         first
         :ship)))

(defn build-swarm-ship
  "Returns a single object to be treated like any other ship. Has a radius and position. Since
  swarms are not circular the radius might not make a ton of sense, but we'll see."
  [ships]
  (let [middle-ship (identify-middle-ship ships)]
    (e/map->Ship {:id (str "swarmer-" (:id middle-ship))
                  :pos (:pos middle-ship)
                  :health (reduce + (map :health ships))
                  :radius (+ e/ship-radius (* how-close-to-swarm (dec (count ships))))
                  :owner-id (:owner-id middle-ship)
                  :docking {:status :undocked}
                  :swarm ships})))

(defn in-swarm-formation?
  "Returns true if the group of ships are already in formation."
  [ships]
  (let [middle-ship (identify-middle-ship ships)]
    (empty? (for [ship ships
                  :let [distance (math/distance-between ship middle-ship)]
                  :when (> distance (max max-swarm-range (* max-swarm-range (/ (count ships) 2))))]
              ship))))

(defn- get-best-swarm-spot
  "Returns the best swarm spot for the swarm and target."
  [swarm target]
  (let [ship-distances (for [ship (:swarm swarm)
                             :let [point (custom-math/get-closest-point-towards-target
                                          ship target e/max-ship-speed)
                                   distance (math/distance-between point target)]]
                         {:ship ship :distance distance :point point})]
    (sort-by :distance ship-distances)))

(defn base-swarm-moves
  "Used for swarms for both attacking and retreating."
  [swarm target navigation-fn retreat?]
  (let [swarm-spot-distances (if retreat?
                               (reverse (get-best-swarm-spot swarm target))
                               (get-best-swarm-spot swarm target))
        ship (-> swarm-spot-distances first :ship)
        orig-point (-> swarm-spot-distances last :point)
        angle (math/orient-towards ship orig-point)
        point (custom-math/get-point ship e/max-ship-speed angle)
        first-move (navigation-fn ship (assoc point :radius 0))]
    (when (and first-move (pos? (:thrust first-move)))
      (map/change-ship-positions! first-move)
      (let [
            updated-ship (get *ships* (:id ship))
            swarm-point updated-ship
            ; swarm-point (custom-math/get-point ship (:thrust first-move) (:angle first-move))
            ; _ (log "Swarming to " swarm-point)
            next-moves (for [next-ship (map :ship swarm-spot-distances)
                             :when (not= (:id ship) (:id next-ship))
                             ; :let [move (navigation/navigate-to next-ship swarm-point)]
                             :let [
                                   ; _ (log "Next ship is" next-ship)
                                   move (navigation/navigate-to-friendly-ship-later
                                         ; next-ship (assoc swarm-point :radius 0)
                                         next-ship swarm-point)]
                             :when move]
                         (do (map/change-ship-positions! move)
                             move))]
        (concat [first-move] next-moves)))))

(defn move-swarm-to-attack
  "Returns moves for the swarm to attack the enemy ship."
  [swarm enemy-ship]
  (base-swarm-moves swarm enemy-ship navigation/navigate-to-specific-point false))

(defn move-swarm-to-retreat
  "Returns moves for the swarm to retreat from the enemy ship."
  [swarm enemy-ship]
  (let [closest-enemy-planet (map/farthest-entity swarm (map/get-planets (:owner-id enemy-ship)))]
    (if closest-enemy-planet
      (base-swarm-moves swarm closest-enemy-planet navigation/navigate-to-retreat true)
      (base-swarm-moves swarm enemy-ship navigation/navigate-to-retreat-ship true))))

(defn get-swarm-move
  "Returns the move for a swarm ship. Retreat range of infinity means don't retreat."
  [swarm enemy-ships retreat-range owner-id]
  ; (log "The swarm is:" (pretty-log swarm))
  (when-let [enemy-ship (map/nearest-enemy-not-decoy swarm enemy-ships)]
    (if (or (= custom-math/infinity retreat-range)
            (> (math/distance-between swarm enemy-ship) retreat-range)
            (map/have-advantage? enemy-ship))
      (move-swarm-to-attack swarm enemy-ship)
      (move-swarm-to-retreat swarm enemy-ship))))

(defn get-swarms
  "Returns a collection of ship swarms."
  [ships]
  (let [swarm-ships (atom nil)]
    (for [ship ships
          :let [valid-ships (remove (fn [ship]
                                      (some #{(:id ship)} @swarm-ships))
                                    ships)
                closeby-ships (filter #(<= (math/distance-between ship %) swarm-eligible-distance-apart)
                                      valid-ships)]
          :when (>= (count closeby-ships) 2)]
      (do
        (swap! swarm-ships concat (map :id closeby-ships))
        (build-swarm-ship closeby-ships)))))
