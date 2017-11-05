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

(defn nearest-enemy-docked-ship
  "TODO"
  [ship docked-enemies]
  (:nearest-ship
   (reduce (fn [{:keys [min-distance nearest-ship]} enemy]
             (let [distance-to-enemy (math/distance-between ship enemy)]
               (if (< distance-to-enemy min-distance)
                 {:min-distance distance-to-enemy :nearest-ship enemy}
                 {:min-distance min-distance :nearest-ship nearest-ship})))
           {:min-distance infinity}
           docked-enemies)))

(defn get-docked-enemy-ships
  "Returns all of the docked enemy ships."
  []
  (let [enemy-ships (remove #(= *player-id* (:owner-id %)) (vals *ships*))]
    (remove #(= :undocked (-> % :docking :status)) enemy-ships)))

(defn move-ship-to-attack
  "Moves the ship to attack the enemy ship."
  [ship enemy-ship]
  (navigation/navigate-to-attack-docked-ship ship enemy-ship))

(defn move-to-nearest-unowned-planet
  "Moves the ship to the nearest unowned planet."
  [ship]
  (when-let [planet (nearest-unowned-planet ship)]
    (move-ship-to-planet ship planet)))

(defn move-to-nearest-enemy-planet
  "Moves the ship to the nearest enemy planet."
  [ship]
  (when-let [planet (nearest-enemy-planet ship)]
    (move-ship-to-planet ship planet)))

(defn move-to-nearest-enemy-ship
  "Moves the ship to the nearest docked enemy ship."
  [ship docked-enemies]
  (when-let [enemy-ship (nearest-enemy-docked-ship ship docked-enemies)]
    (move-ship-to-attack ship enemy-ship)))

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

(comment
 (vals {})
 (apply max 0 (map count {})))

; (defn get-pesky-fighters
;   "Fighters near my planets or neutral planets."
;   []
;   (let [mine-or-neutral-planets (filter #(or (nil? (:owner-id %))
;                                              (= *player-id* (:owner-id %))))
;         close-distance 25
;         filter-fn (fn [ship]
;                     (and (< (math/distance-between ship planet) close-distance)
;                          (= :undocked (-> ship :docking :status))))]
;     (mapcat #(filter filter-fn (vals *ships*)) mine-or-neutral-planets)))

(defn get-pesky-fighters
  "Fighters near my planets or neutral planets."
  []
  (let [mine-or-neutral-planets (filter #(or (nil? (:owner-id %))
                                             (= *player-id* (:owner-id %)))
                                        (vals *planets*))
        close-distance 25]
        ; filter-fn (fn [ship]
        ;             (and (< (math/distance-between ship planet) close-distance)
        ;                  (= :undocked (-> ship :docking :status))))]
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
                    (and (< (math/distance-between ship planet) close-distance)
                         (= :undocked (-> ship :docking :status))))
        nearby-fighters (filter filter-fn (vals *ships*))
        fighters-by-owner (group-by :owner-id nearby-fighters)
        ; _ (log "FBO:" fighters-by-owner)
        max-count (apply max 0 (map count (vals fighters-by-owner)))]
        ; _ (log "Count can't be called on:" (get fighters-by-owner *player-id* 0))]
    (= max-count (count (get fighters-by-owner *player-id* [])))))

(defn safe-docking-planets
  "Returns a list of planets that are safe to dock at."
  []
  (let [filter-fn (fn [planet]
                    (and (or (nil? (:owner-id planet))
                             (and (= *player-id* (:owner-id planet))
                                  (e/any-remaining-docking-spots? planet)))
                         (have-most-ships-surrounding-planet? planet)))]
    (filter filter-fn (vals *planets*))))

(defn compute-move-closest-planet
  "New one"
  [ship unowned? dock-spots? docked-enemies safe-planets pesky-fighters start-ms]
  (let [times-up? (> (- (System/currentTimeMillis) start-ms) 1700)]
    (if (or times-up?
            (not= :undocked (-> ship :docking :status)))
      nil
      (when-let [planets (filter #(or (nil? (:owner-id %))
                                      (and (= *player-id* (:owner-id %))
                                           (e/any-remaining-docking-spots? %))
                                      (and (not (nil? (:owner-id %)))
                                           (not= *player-id* (:owner-id %))))
                                 (vals *planets*))]
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
                   (some #{(:id nearest-planet)} safe-planets))
                  ;  (or (nil? (:owner-id nearest-planet))
                      ;  (= *player-id* (:owner-id nearest-planet)))
            (move-ship-to-planet ship nearest-planet)
            (if (or (nil? (:owner-id nearest-planet))
                    (= *player-id* (:owner-id nearest-planet)))
              (move-to-nearest-enemy-ship ship pesky-fighters)
              (move-to-nearest-enemy-ship ship (concat pesky-fighters docked-enemies)))))))))

(defn compute-move
  "Returns the move for the given ship"
  [ship unowned? dock-spots? docked-enemies start-ms]
  (let [times-up? (> (- (System/currentTimeMillis) start-ms) 1700)]
    (if (or times-up?
            (not= :undocked (-> ship :docking :status)))
      nil
      (if dock-spots?
        (if-let [planet (nearest-open-dock-spot ship)]
          (move-ship-to-planet ship planet)
          (if unowned?
            (if-let [planet (nearest-unowned-planet ship)]
              (move-ship-to-planet ship planet)
              (move-to-nearest-enemy-ship ship docked-enemies))
            (move-to-nearest-enemy-ship ship docked-enemies)))
        (if unowned?
          (if-let [planet (nearest-unowned-planet ship)]
            (move-ship-to-planet ship planet)
            (move-to-nearest-enemy-ship ship docked-enemies))
          (move-to-nearest-enemy-ship ship docked-enemies))))))

(defn compute-move-orig
  "Returns the move for the given ship"
  [ship unowned? dock-spots? docked-enemies start-ms]
  (let [times-up? (> (- (System/currentTimeMillis) start-ms) 1700)]
    (if (or times-up?
            (not= :undocked (-> ship :docking :status)))
      nil
      (if unowned?
        (if-let [planet (nearest-unowned-planet ship)]
          (move-ship-to-planet ship planet)
          (if dock-spots?
            (if-let [planet (nearest-open-dock-spot ship)]
              (move-ship-to-planet ship planet)
              (move-to-nearest-enemy-ship ship docked-enemies))
            (move-to-nearest-enemy-ship ship docked-enemies)))
        (if dock-spots?
          (if-let [planet (nearest-open-dock-spot ship)]
            (move-ship-to-planet ship planet)
            (move-to-nearest-enemy-ship ship docked-enemies))
          (move-to-nearest-enemy-ship ship docked-enemies))))))

(defn get-num-planets-owned
  "Returns how many planets I own."
  []
  (count (filter #(= *player-id* (:owner-id %)) (vals *planets*))))

(defn -main
  [& args]
  (initialize-game
   (io/send-done-initialized)
   (doseq [turn (iterate inc 1)]
     (with-updated-map
      (let [unowned? (any-unowned-planets?)
            num-planets (get-num-planets-owned)
            dock-spots? (any-dock-spots-available?)
            docked-enemies (get-docked-enemy-ships)
            safe-planets (map :id (safe-docking-planets))
            pesky-fighters (get-pesky-fighters)
            _ (log "==== Turn" turn "==== Unowned" unowned? "==== Dock spots" dock-spots?
                   "==== Num planets" num-planets "==== Safe planets" safe-planets
                   "=== Pesky " pesky-fighters)
            start-ms (System/currentTimeMillis)
            ; compute-move-fn (if (< num-planets 3) compute-move-orig compute-move)
            compute-move-fn compute-move-closest-planet
            moves (keep #(compute-move-fn % unowned? dock-spots? docked-enemies safe-planets
                                          pesky-fighters start-ms)
                        (vals (get *owner-ships* *player-id*)))]
        (io/send-moves moves))))))
