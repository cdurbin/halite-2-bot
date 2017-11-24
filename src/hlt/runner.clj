(ns hlt.runner
  (:require [clojure.java.io :as clj.io]
            [hlt.networking :as io]
            [hlt.game-map :refer [*player-id* *map-size* *bot-name*
                                  *owner-ships* *ships* *planets*]]
            [hlt.utils :as utils :refer [log]]
            [custom.durbinator :as durbinator]
            [custom.game-map :refer [*safe-planets* *docked-enemies* *pesky-fighters* *num-ships*
                                     *num-players*]]
            [custom.map-analysis :as map]
            [custom.utils :refer [defn-timed]])

  (:import (java.io PrintWriter))
  (:gen-class))

(defmacro with-updated-map
  [& body]
  `(let [m# (io/read-map)]
     (binding [*owner-ships* (:owner-ships m#)
               *ships* (:ships m#)
               *planets* (:planets m#)
               *safe-planets* nil
               *docked-enemies* nil
               *pesky-fighters* nil
               *num-ships* 0
               *num-players* 0]
       ~@body)))

(defmacro initialize-game
  [& body]
  `(let [prelude# (io/read-prelude)
         bot-name# (str durbinator/my-bot-name "-" (:player-id prelude#))]
     (with-open [logger# (clj.io/writer (str bot-name# ".log") :append true)]
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

;; Hooks to track per turn custom map info and calling the custom function for determining a move
;; for a ship
(def compute-move-fn durbinator/compute-move-closest-planet)
; (def compute-move-fn durbinator/compute-move-best-planet)
(def custom-map-info-fn durbinator/calculations-for-turn)
(def ships-in-order-fn map/sort-ships-by-distance)

(defn-timed all-moves
  [ships-in-order custom-map-info]
  (doall
   (keep #(compute-move-fn custom-map-info %)
         ships-in-order)))

(defn -main
  [& args]
  (initialize-game
   (durbinator/starting-game-strategy!)
   (io/send-done-initialized)
   (doseq [turn (iterate inc 1)]
     (with-updated-map
      (do
        (log "==== Turn" turn)
        (let [custom-map-info (custom-map-info-fn turn)
              ships-in-order (ships-in-order-fn (vals (get *owner-ships* *player-id*)))
              runaway-moves (durbinator/run-to-corner-moves (reverse ships-in-order))
              moving-ships (map #(get-in % [:ship :id]) runaway-moves)
              ; initial-moves (durbinator/attack-unprotected-enemy-ships moving-ships)
              ; moving-ships (map #(get-in % [:ship :id]) (concat initial-moves runaway-moves))
              defend-moves (durbinator/defend-vulnerable-ships moving-ships)
              moving-ships (map #(get-in % [:ship :id]) (concat defend-moves runaway-moves))
              initial-moves (durbinator/attack-unprotected-enemy-ships moving-ships)
              moving-ships (map #(get-in % [:ship :id]) (concat runaway-moves initial-moves defend-moves))
              ; _ (log "==== Moving ships" (pr-str moving-ships))
              ; moves (keep #(compute-move-fn (assoc custom-map-info :moving-ships moving-ships) %)
              ;             ships-in-order)
              moves (all-moves ships-in-order (assoc custom-map-info :moving-ships moving-ships))
              friendly-moves (filter #(= :friendly (:subtype %)) moves)
              moves (remove #(= :friendly (:subtype %)) moves)
              moves (remove #(= 0 (:thrust %)) moves)
              friendly-moves (durbinator/recalculate-friendly-moves friendly-moves)]
          ; (log "Runaway moves" runaway-moves)
          ; (log "=== Defend moves:" defend-moves)
          ; (log "Initial moves:" initial-moves)
          ; (log "Moves:" moves)
          ; (log "Friendly" friendly-moves)
          ; (io/send-moves (concat defend-moves initial-moves moves))))))))
          (io/send-moves (concat runaway-moves defend-moves initial-moves moves friendly-moves))))))))
