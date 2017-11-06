(ns custom.game-map)

(def ^:dynamic *pesky-fighters*
  "A list of ships to battle against because they are too close to my planet."
  nil)

(def ^:dynamic *safe-planets*
  "A list of planets that are safe to dock against."
  nil)

(def ^:dynamic *docked-enemies*
  "A list of enemy ships that are currently docked (or in the processing of docking or undocking)"
  nil)
