(ns custom.math
  "Custom math functions."
  (:require
   [hlt.math :as hlt-math]
   [hlt.utils :refer [log]]))

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

(defn final-position-radians
  "Returns the final position based on an angle and magnitude."
  [x y thrust angle]
  (let [x-magnitude (* thrust (Math/cos angle))
        y-magnitude (* thrust (Math/sin angle))]
    {:x (+ x x-magnitude)
     :y (+ y y-magnitude)}))

(defn all-positions-start-to-end
  "Returns multiple positions (one for each integer) from starting spot to ending spot."
  [x y thrust angle]
  (for [n (range 1 (inc thrust) 1)]
    (hlt-math/map->Position (final-position x y n angle))))

;; Need 7 different distances, 1/7th of the total thrust, 2/7th, ...
(defn all-positions-start-to-end-new
  "Returns multiple positions (one for each integer) from starting spot to ending spot."
  [x y thrust angle]
  (into {}
        (for [n (range 1 7.1)]
          [n (hlt-math/map->Position (final-position x y (* n (/ thrust 7)) angle))])))

(defn all-positions-start-to-end-radians
  "Returns multiple positions (one for each integer) from starting spot to ending spot."
  [x y thrust angle]
  (for [n (range 1 7.1)]
    (hlt-math/map->Position (final-position-radians x y (* n (/ thrust 7)) angle))))

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
  ; (log "Am I crazy"
  ;      "Pos" pos
  ;      "Distance" distance
  ;      "Angle" angle)
  (let [x (+ (hlt-math/get-x pos) (* distance (Math/cos angle)))
        y (+ (hlt-math/get-y pos) (* distance (Math/sin angle)))]
    ; (log "Returning" (hlt-math/->Position x y))
    (hlt-math/->Position x y)))

(defn get-point-between
  "Returns a point between the two positions at percent."
  [start end multiplier]
  (let [angle (hlt-math/orient-towards start end)
        distance (* multiplier (hlt-math/distance-between start end))]
    (get-point start distance angle)))

(defn get-closest-point-towards-target
  "Returns the closest point towards the target."
  [start target distance]
  (let [angle (hlt-math/orient-towards start target)]
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

(defn calculate-end-positions
  "Returns a ship in its end position based on thrust for this turn."
  [{:keys [ship thrust angle]}]
  (let [x (get-in ship [:pos :x])
        y (get-in ship [:pos :y])
        positions (all-positions-start-to-end-new x y thrust angle)]
    (map (fn [[k v]]
           (assoc ship :pos v :turn k))
         positions)))

(defn get-in-turn-segments
  "Get all the mid-turn start and end positions."
  [{:keys [ship thrust angle]}]
  (let [x (get-in ship [:pos :x])
        y (get-in ship [:pos :y])
        positions (all-positions-start-to-end-radians x y thrust angle)]
    (reduce (fn [old-map new-value]
              ; (println "OLD " old-map)
              (let [max-key (apply max (keys old-map))
                    start (get-in old-map [max-key :end])]
                ; (println "Max" max-key)
                ; (println "Start" start)
                (merge old-map {(inc max-key)
                                {:start start
                                 :end new-value}})))
            {0
             {:end (:pos ship)}}
            positions)))




(comment
 (range 1 (+ 0.2 7) 2)
 (all-positions-start-to-end 10 3 7 15)
 (get-in-turn-segments {:ship {:pos {:x 10 :y 3}} :thrust 7 :angle 15})
 (final-position 10 3 7 15)
 (final-position 10 3 7 0)
 (final-position 10 3 7 360)
 (let [x1 96.729
       y1 86.596
       x2 99.782
       y2 81.113]
   (hlt-math/distance-between (hlt-math/->Position x1 y1) (hlt-math/->Position x2 y2))))
