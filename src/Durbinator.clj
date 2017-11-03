(ns Durbinator
  (:require [clojure.java.io :as clj.io]
            [hlt.networking :as io]
            [hlt.game-map :refer [*player-id* *map-size* *bot-name*
                                  *owner-ships* *ships* *planets*]]
            [hlt.utils :as utils :refer [log]]
            [hlt.entity :as e]
            [hlt.math :as math]
            [hlt.navigation :as navigation])
  (:import (java.io PrintWriter))
  (:gen-class))

(defmacro with-updated-map
  [& body]
  `(let [m# (io/read-map)]
     (binding [*owner-ships* (:owner-ships m#)
               *ships* (:ships m#)
               *planets* (:planets m#)]
       ~@body)))

(def my-bot-name "Durbinator")

(defmacro initialize-game
  [& body]
  `(let [prelude# (io/read-prelude)
         bot-name# (str my-bot-name "-" (:player-id prelude#))]
     (with-open [logger# (clj.io/writer (str bot-name# ".log"))]
       (binding [utils/*logger* logger#
                 *bot-name* bot-name#
                 *player-id* (:player-id prelude#)
                 *map-size* (:map-size prelude#)]
         (try
           (with-updated-map ~@body)
           (catch Throwable t#
             (with-open [pw# (PrintWriter. utils/*logger*)]
               (.printStackTrace t# pw#))
             (throw t#)))))))

(def infinity 99999999)

(defn nearest-unowned-planet
  "Returns the nearest planet to the given ship."
  [ship]
  (:nearest-planet
   (reduce (fn [{:keys [min-distance nearest-planet]} planet-tuple]
             (let [planet (second planet-tuple)
                   distance-to-planet (math/distance-between ship planet)]
               (if (and (nil? (:owner-id planet)) (< distance-to-planet min-distance))
                 {:min-distance distance-to-planet :nearest-planet planet}
                 {:min-distance min-distance :nearest-planet nearest-planet})))
           {:min-distance infinity}
           *planets*)))

(defn nearest-enemy-planet
  "Returns the nearest planet to the given ship."
  [ship]
  (:nearest-planet
   (reduce (fn [{:keys [min-distance nearest-planet]} planet-tuple]
             (let [planet (second planet-tuple)
                   distance-to-planet (math/distance-between ship planet)]
               (if (and (not (nil? (:owner-id planet)))
                        (not (= *player-id* (:owner-id planet)))
                        (< distance-to-planet min-distance))
                 {:min-distance distance-to-planet :nearest-planet planet}
                 {:min-distance min-distance :nearest-planet nearest-planet})))
           {:min-distance infinity}
           *planets*)))

(defn move-ship-to-planet
  "Moves the ship to the given planet."
  [ship planet]
  (if (e/within-docking-range? ship planet)
    (e/dock-move ship planet)
    (navigation/navigate-to-dock ship planet)))

(defn move-to-nearest-unowned-planet
  "Moves the ship to the nearest unowned planet."
  [ship]
  (when-let [planet (nearest-unowned-planet ship)]
    (move-ship-to-planet ship planet)))

(defn move-to-nearest-enemy-planet
  "Moves the ship to the nearest unowned planet."
  [ship]
  (when-let [planet (nearest-enemy-planet ship)]
    (move-ship-to-planet ship planet)))

(defn nearest-open-dock-spot
  "Returns the planet owned by me with the nearest open dock spot."
  [ship]
  (:nearest-planet
   (reduce (fn [{:keys [min-distance nearest-planet]} planet-tuple]
             (let [planet (second planet-tuple)
                   distance-to-planet (math/distance-between ship planet)]
               (if (and (= *player-id* (:owner-id planet))
                        (e/any-remaining-docking-spots? planet)
                        (< distance-to-planet min-distance))
                 {:min-distance distance-to-planet :nearest-planet planet}
                 {:min-distance min-distance :nearest-planet nearest-planet})))
           {:min-distance infinity}
           *planets*)))

(defn any-unowned-planets?
  "Returns all the unowned planets."
  []
  (some? (seq (filter #(nil? (:owner-id %)) (vals *planets*)))))

(defn any-dock-spots-available?
  "Returns true if I own any planet with an open docking spot."
  []
  (some?
   (seq
    (filter #(and (= *player-id* (:owner-id %))
                  (e/any-remaining-docking-spots? %))
            (vals *planets*)))))

(defn compute-move
  "Returns the move for the given ship"
  [ship unowned? dock-spots?]
  (if (not= :undocked (-> ship :docking :status))
    nil
    (if unowned?
      (if-let [planet (nearest-unowned-planet ship)]
        (move-ship-to-planet ship planet)
        (if dock-spots?
          (if-let [planet (nearest-open-dock-spot ship)]
            (move-ship-to-planet ship planet)
            (move-to-nearest-enemy-planet ship))
          (move-to-nearest-enemy-planet ship)))
      (if dock-spots?
        (if-let [planet (nearest-open-dock-spot ship)]
          (move-ship-to-planet ship planet)
          (move-to-nearest-enemy-planet ship))
        (move-to-nearest-enemy-planet ship)))))

(defn -main
  [& args]
  (initialize-game
   (io/send-done-initialized)
   (doseq [turn (iterate inc 1)]
     (with-updated-map
      (let [unowned? (any-unowned-planets?)
            dock-spots? (any-dock-spots-available?)
            _ (log "=========== Turn" turn "=========== Unowned" unowned? "===== Dock spots" dock-spots?)
            moves (keep #(compute-move % unowned? dock-spots?)
                        (vals (get *owner-ships* *player-id*)))]
        (io/send-moves moves))))))
