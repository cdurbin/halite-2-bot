(ns custom.durbinator
  (:require
   [custom.game-map :refer [*safe-planets* *docked-enemies* *pesky-fighters*]]
   [custom.math :as custom-math]
   [custom.navigation :as navigation]
   [custom.utils :as utils]
   [hlt.entity :as e]
   [hlt.game-map :refer [*player-id* *map-size* *bot-name* *owner-ships* *ships* *planets*]]
   [hlt.math :as math]
   [hlt.utils :refer [log]])
  (:import (java.io PrintWriter)))

(def my-bot-name "Durbinator")

(def infinity 99999999)

(defn move-ship-to-planet!
  "Moves the ship to the given planet. Side effect to update the planet to reduce the number of
  available docking spots by one."
  [ship planet]
  (let [upd-planet (update-in planet [:docking :ships] conj ship)]
    (if-not (e/any-remaining-docking-spots? upd-planet)
      (set! *safe-planets* (dissoc *safe-planets* (:id planet)))
      (set! *safe-planets* (assoc *safe-planets* (:id planet) upd-planet))))
  (if (e/within-docking-range? ship planet)
    (e/dock-move ship planet)
    (navigation/navigate-to-dock ship planet)))

(defn nearest-enemy-ship
  "Returns the closest enemy ship from the passed in enemy ships."
  [ship enemy-ships]
  (:nearest-ship
   (reduce (fn [{:keys [min-distance nearest-ship]} enemy]
             (let [distance-to-enemy (math/distance-between ship enemy)]
               (if (< distance-to-enemy min-distance)
                 {:min-distance distance-to-enemy :nearest-ship enemy}
                 {:min-distance min-distance :nearest-ship nearest-ship})))
           {:min-distance infinity}
           enemy-ships)))

(defn get-docked-enemy-ships
  "Returns all of the docked enemy ships."
  []
  (let [enemy-ships (remove #(= *player-id* (:owner-id %)) (vals *ships*))]
    (remove #(= :undocked (-> % :docking :status)) enemy-ships)))

(defn move-ship-to-attack
  "Moves the ship to attack the enemy ship."
  [ship enemy-ship]
  (navigation/navigate-to-attack-ship ship enemy-ship))

(defn move-to-nearest-enemy-ship
  "Moves the ship to the nearest docked enemy ship."
  [ship enemy-ships]
  (when-let [enemy-ship (nearest-enemy-ship ship enemy-ships)]
    (move-ship-to-attack ship enemy-ship)))

(defn get-pesky-fighters
  "Fighters near my planets or neutral planets."
  []
  (let [mine-or-neutral-planets (filter #(or (nil? (:owner-id %))
                                             (= *player-id* (:owner-id %)))
                                        (vals *planets*))
        close-distance (* 7 8)]
    (set
      (for [planet mine-or-neutral-planets
            ship (vals *ships*)
            :when (and (< (math/distance-between ship planet) close-distance)
                       (= :undocked (-> ship :docking :status))
                       (not= *player-id* (:owner-id ship)))]
        ship))))

(defn have-most-ships-surrounding-planet?
  "Have the most fighters (non docking) surrounding the planet."
  [planet]
  (let [close-distance 20
        filter-fn (fn [ship]
                    (and (< (math/distance-between ship planet) (+ close-distance (:radius planet)))
                         (= :undocked (-> ship :docking :status))))
        nearby-fighters (filter filter-fn (vals *ships*))
        fighters-by-owner (group-by :owner-id nearby-fighters)
        my-count (count (get fighters-by-owner *player-id*))
        max-other-count (apply max 0 (map count (vals (dissoc fighters-by-owner *player-id*))))]
    (or (zero? max-other-count)
        (> my-count (inc max-other-count)))))

(defn get-safe-planets
  "Returns a list of planets that are safe to dock at."
  []
  (let [filter-fn (fn [planet]
                    (and (or (nil? (:owner-id planet))
                             (and (= *player-id* (:owner-id planet))
                                  (e/any-remaining-docking-spots? planet)))
                         (have-most-ships-surrounding-planet? planet)))]
    (filter filter-fn (vals *planets*))))

(defn compute-move-closest-planet*
  "Picks the move for the ship based on proximity to planets and fighters near planets."
  [{:keys [start-ms]} ship]
  (let [times-up? (> (- (System/currentTimeMillis) start-ms) 1700)]
    (if (or times-up?
            (not= :undocked (-> ship :docking :status)))
      nil
      (when-let [planets (concat (vals *safe-planets*)
                                 (filter #(and (not (nil? (:owner-id %)))
                                               (not= *player-id* (:owner-id %)))
                                         (vals *planets*)))]
        (let [nearest-planet
              (:nearest-planet
               (reduce (fn [{:keys [min-distance nearest-planet]} planet]
                         (let [distance-to-planet (math/distance-between ship planet)]
                           (if (< distance-to-planet min-distance)
                             {:min-distance distance-to-planet :nearest-planet planet}
                             {:min-distance min-distance :nearest-planet nearest-planet})))
                       {:min-distance infinity}
                       planets))]
          (if (and nearest-planet
                   (some #{(:id nearest-planet)} (keys *safe-planets*)))
            (move-ship-to-planet! ship nearest-planet)
            (if (or (nil? (:owner-id nearest-planet))
                    (= *player-id* (:owner-id nearest-planet)))
              (move-to-nearest-enemy-ship ship (concat *pesky-fighters* *docked-enemies*))
              (move-to-nearest-enemy-ship ship (concat *pesky-fighters* *docked-enemies*)))))))))

(defn calculate-end-positions
  "Returns a ship in its end position based on thrust for this turn."
  [{:keys [ship thrust angle] :as move}]
  (let [x (get-in ship [:pos :x])
        y (get-in ship [:pos :y])
        positions (map math/map->Position
                       (custom-math/all-positions-start-to-end x y thrust angle))]
    (map #(assoc ship :pos %) positions)))

(defn change-ship-positions
  "Changes a ships position in the main ships."
  [{:keys [ship type] :as move}]
  (when (= :thrust type)
    (let [imaginary-ships (calculate-end-positions move)]
      (doseq [i-ship imaginary-ships]
        (set! *ships* (assoc *ships* (java.util.UUID/randomUUID) i-ship))))))

(defn compute-move-closest-planet
  "Picks the move for the ship based on proximity to planets and fighters near planets."
  [{:keys [start-ms] :as custom-map-info} ship]
  (log "Computing move for " ship)
  (when-let [move (compute-move-closest-planet* custom-map-info ship)]
    (change-ship-positions move)
    move))

(defn get-custom-map-info
  "Returns additional map info that is useful to calculate at the beginning of each turn."
  []
  (set! *safe-planets* (into {} (map (fn [planet] [(:id planet) planet]) (get-safe-planets))))
  (set! *docked-enemies* (get-docked-enemy-ships))
  (set! *pesky-fighters* (get-pesky-fighters))
  {:start-ms (System/currentTimeMillis)})

(defn distance-to-poi
  "Returns the smallest distance to any POI."
  [ship points-of-interest]
  (reduce (fn [min-distance poi]
            (let [distance-to-poi (math/distance-between ship poi)]
              (if (< distance-to-poi min-distance)
                distance-to-poi
                min-distance)))
          infinity
          points-of-interest))

(defn sort-ships-by-distance
  "Returns ships from closest to point of interest to farthest. A point of interest is a planet,
  docked enemy ship, enemy ship near one of my planets."
  [ships]
  (let [pois (concat *docked-enemies* (vals *safe-planets*) *pesky-fighters*)
        ships-w-distance (map #(assoc % :distance (distance-to-poi % pois)) ships)]
    (map #(dissoc % :distance) (sort (utils/compare-by :distance utils/asc) ships-w-distance))))
