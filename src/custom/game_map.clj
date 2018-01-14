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

(def ^:dynamic *num-ships*
  "Number of ships I own."
  0)

(def ^:dynamic *num-players*
  "Number of players left with ships."
  0)

(def ^:dynamic *attack-spots*
  "Locations to attack."
  nil)

(def ^:dynamic *start-ms*
  "Starting ms for the round to know when to time out."
  0)

(def ^:dynamic *spawn-points*
  "A list of points enemies might spawn from."
  nil)
