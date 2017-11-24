(ns hlt.runner
  (:require [clojure.java.io :as clj.io]
            [hlt.networking :as io]
            [hlt.game-map :refer [*player-id* *map-size* *bot-name*
                                  *owner-ships* *ships* *planets*]]
            [hlt.utils :as utils :refer [log]]
            [custom.durbinator :as durbinator]
            [custom.game-map :refer [*safe-planets* *docked-enemies* *pesky-fighters* *num-ships*
                                     *num-players*]]
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
(def get-moves-for-turn-fn durbinator/get-moves-for-turn)

(defn -main
  [& args]
  (initialize-game
   (durbinator/starting-game-strategy!)
   (io/send-done-initialized)
   (doseq [turn (iterate inc 1)]
     (with-updated-map
      (do
        (log "==== Turn" turn)
        (let [moves (get-moves-for-turn-fn turn)]
          (io/send-moves moves)))))))
