(ns custom.math
  "Custom math functions."
  (:require
   [hlt.math :as hlt-math]))

(def infinity 99999999)

(defn deg->rad
  "Translates degrees to radians."
  [deg]
  (Math/toRadians deg))

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

(defn get-point-between
  "Returns a point between the two positions at percent."
  [start end multiplier]
  (let [angle (hlt-math/orient-towards start end)
        distance (* multiplier (hlt-math/distance-between start end))]
    (get-point start distance angle)))

; (defn get-values-for-segment-circle-intersects
;   "Optimization"
;   [p1 p2]
;   (let [x1 (get-x p1)
;         y1 (get-y p1)
;         x2 (get-x p2)
;         y2 (get-y p2)
;         dx (- x2 x1)
;         dy (- y2 y1)
;         a (+ (square dx) (square dy))]
;     {:a a :dx dx :dy dy}))
;
;
; (defn faster-segment-circle-intersects?
;   "Returns true if the entity intersects with the line segment between p1 and p2,
;   false otherwise. An optional fudge factor can be used to ensure a
;   little padding around entities that may move, to reduce the
;   probability of hitting into them."
;   ([entity {:keys [a dx dy] :as params} (segment-circle-intersects? entity params 0.0)])
;   ([entity {:keys [a dx dy]} fudge-factor]
;    ;; Parameterize the segment as start + t * (end - start),
;    ;; and substitute into the equation of a circle
;    ;; Solve for t
;    (let [fudged-radius (+ (:radius entity) fudge-factor)
;          x1 (get-x p1)
;          y1 (get-y p1)
;          x2 (get-x p2)
;          y2 (get-y p2)
;          dx (- x2 x1)
;          dy (- y2 y1)
;          a (+ (square dx) (square dy))
;
;          center-x (get-x entity)
;          center-y (get-y entity)
;
;          b (* -2 (+ (square x1) (- (* x1 x2))
;                     (- (* x1 center-x)) (* center-x x2)
;                     (square y1) (- (* y1 y2))
;                     (- (* y1 center-y)) (* center-y y2)))]
;      (if (== a 0.0)
;        ;; start == end
;        (<= (distance-between p1 entity) fudged-radius)
;        ;; time along segment when closest to the circle (vertex of the quadratic)
;        (let [t (min (/ (- b) (* 2 a)) 1.0)]
;          (if (< t 0)
;            false
;            (let [closest-x (+ x1 (* dx t))
;                  closest-y (+ y1 (* dy t))
;                  closest-distance (distance-between (->Position closest-x closest-y)
;                                                     entity)]
;              (<= closest-distance fudged-radius))))))))

(comment
 (range 1 (+ 0.2 7) 2)
 (all-positions-start-to-end 10 3 7 15)
 (final-position 10 3 7 15)
 (final-position 10 3 7 0)
 (final-position 10 3 7 360))
