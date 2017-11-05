(ns custom.math
  "Custom math functions.")

(defn deg->rad
  "Translates degrees to radians."
  [deg]
  (Math/toRadians deg))

(defn final-position
  "Returns the final position based on an angle and magnitude."
  [x y thrust angle]
  (let [angle-rad (deg->rad angle)
        x-magnitude (* thrust (Math/cos angle-rad))
        y-magnitude (* thrust (Math/sin angle-rad))]
    {:x (+ x x-magnitude)
     :y (+ y y-magnitude)}))

(defn integers-up-to
  "Returns a collection of integers between 1 and n."
  [n]
  (let [i (atom 1)
        ints (atom [])]
    (while (<= @i n)
      (swap! ints conj @i)
      (swap! i inc))
    @ints))

(defn all-positions-start-to-end
  "Returns multiple positions from starting spot to ending spot."
  [x y thrust angle]
  (for [n (integers-up-to thrust)]
    (final-position x y n angle)))


(comment
 (integers-up-to 7)
 (all-positions-start-to-end 10 3 7 15)
 (final-position 10 3 7 15)
 (final-position 10 3 7 0)
 (final-position 10 3 7 360))
