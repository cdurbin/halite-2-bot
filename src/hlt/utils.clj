(ns hlt.utils
  (:require [clojure.string :as str]))

(def ^:dynamic *logger* nil)

(def log-stuff (atom false))

(defn log
  "Logs the arguments to the log file, as if printed by println.
  Multiple arguments are separated by spaces."
  [& args]
  (when @log-stuff
    (binding [*out* *logger*]
      (apply println args))))
