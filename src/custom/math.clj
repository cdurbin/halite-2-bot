(ns custom.math
  "Custom math functions."
  (:require
   [hlt.math :as hlt-math]))

(defn deg->rad
  "Translates degrees to radians."
  [deg]
  (Math/toRadians deg))

(comment
 (hlt-math/rad->deg (/ Math/PI 3))
 (hlt-math/rad->deg (/ Math/PI -3))
 (hlt-math/rad->deg 0)

 (between (hlt-math/rad->deg (+ Math/PI 5)) (hlt-math/rad->deg (/ Math/PI -3))
          (hlt-math/rad->deg (+ Math/PI 5))))

(defn between
  "Returns whether the given angle is between two angles"
  [plus-angle minus-point-angle compare-angle]
  (let [upd-plus-angle (if (> minus-point-angle plus-angle)
                         (+ 360 plus-angle)
                         plus-angle)
        upd-compare-angle (if (>= upd-plus-angle 360)
                            (+ 360 compare-angle)
                            compare-angle)]
    (<= minus-point-angle upd-compare-angle upd-plus-angle)))



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
  (for [n (range 1 (+ 0.2 thrust) 1)]
    (final-position x y n angle)))

(defn orient-away
  "Returns the opposite direction angle from `from` to `to` in radians."
  [from to]
  (let [dx (- (hlt-math/get-x to) (hlt-math/get-x from))
        dy (- (hlt-math/get-y to) (hlt-math/get-y from))]
    (+ (Math/atan2 dy dx)
       Math/PI)))

(defn get-point
  "Returns the point that is distance away at the provided angle from the current point."
  [pos distance angle]
  (let [x (+ (hlt-math/get-x pos) (* distance (Math/cos angle)))
        y (+ (hlt-math/get-y pos) (* distance (Math/sin angle)))]
    (hlt-math/->Position x y)))

(comment
 (range 1 (+ 0.2 7) 2)
 (all-positions-start-to-end 10 3 7 15)
 (final-position 10 3 7 15)
 (final-position 10 3 7 0)
 (final-position 10 3 7 360))
