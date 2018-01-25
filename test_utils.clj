(ns test-utils
  "Test helper functions.")

(defn round2
  "Round a double to the given precision (number of significant digits)"
  [d precision]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))

(defn almost=
  "Checks if things are really close to being equal."
  [expected actual]
  (let [rounded-actual (into {}
                             (map (fn [[k v]]
                                    [k (round2 v 5)])
                                  actual))
        rounded-expected (into {}
                               (map (fn [[k v]]
                                      [k (round2 v 5)])
                                    expected))]
    (= rounded-expected rounded-actual)))

(defn entries-almost=
  "Checks if things are really close to being equal."
  [expected-entries actual-entries])

(defn round-positions-map
  "Round the x and y value of position maps."
  [position-maps]
  (map (fn [[k v]]
         (let [the-x (:x v)
               the-y (:y v)]
           {k {:x (round2 the-x 5)
               :y (round2 the-y 5)}}))
       position-maps))

(defn round-positions-vec
  "Round the x and y value of position maps."
  [positions]
  (map (fn [v]
         (let [the-x (:x v)
               the-y (:y v)]
           {:x (round2 the-x 5)
            :y (round2 the-y 5)}))
       positions))
