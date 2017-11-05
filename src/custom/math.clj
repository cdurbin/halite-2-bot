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

(defn all-positions-start-to-end
  "Returns multiple positions (one for each integer) from starting spot to ending spot."
  [x y thrust angle]
  (for [n (range 1 (+ 0.2 thrust) 2)]
    (final-position x y n angle)))

(comment
 (range 1 (+ 0.2 7) 2)
 (all-positions-start-to-end 10 3 7 15)
 (final-position 10 3 7 15)
 (final-position 10 3 7 0)
 (final-position 10 3 7 360))
