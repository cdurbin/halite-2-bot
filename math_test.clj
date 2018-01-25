(ns math_test
  "Tests for the custom math functions."
  (:require
   [clojure.test :refer :all]
   [test-utils :refer :all]
   [custom.math :as custom-math]
   [hlt.math :as hlt-math]))

(deftest deg->rad
  (testing "Various degree to radian tests"
    (are
      [degrees radians]
      (= radians (custom-math/deg->rad degrees))

      180 Math/PI
      540 Math/PI
      90 (/ Math/PI 2)
      45 (/ Math/PI 4)
      270 (* Math/PI 1.5)
      0 0.0
      360 0.0
      720 0.0)))

(deftest final-position-radians
  (testing "Final position radians test"
    (are
      [x y thrust angle final-x final-y]
      (almost= {:x final-x :y final-y}
               (custom-math/final-position-radians x y thrust angle))

      0 0 1 Math/PI -1 0
      1 1 1 Math/PI 0 1
      0 0 7 (/ Math/PI 2) 0 7
      0 0 1 (* Math/PI 6) 1 0
      10 10 5 (* Math/PI 6) 15 10)))

(deftest final-position-degrees
  (testing "Final position degrees test"
    (are
      [x y thrust angle final-x final-y]
      (almost= {:x final-x :y final-y}
               (custom-math/final-position-degrees x y thrust angle))

      0 0 1 180 -1 0
      1 1 1 180 0 1
      0 0 7 90 0 7
      0 0 1 0 1 0
      10 10 5 360 15 10)))

(deftest all-positions-start-to-end
  (testing "All positions start to end"
    (are
      [x y thrust angle intermittent-positions]
      (let [actual-entries (custom-math/all-positions-start-to-end x y thrust angle)
            actual (round-positions-map actual-entries)
            expected (round-positions-map intermittent-positions)]
        (= expected actual))

      0 0 7 180
      {1 (hlt-math/map->Position {:x -1 :y 0})
       2 (hlt-math/map->Position {:x -2 :y 0})
       3 (hlt-math/map->Position {:x -3 :y 0})
       4 (hlt-math/map->Position {:x -4 :y 0})
       5 (hlt-math/map->Position {:x -5 :y 0})
       6 (hlt-math/map->Position {:x -6 :y 0})
       7 (hlt-math/map->Position {:x -7 :y 0})})))

(deftest all-positions-start-to-end-radians
  (testing "All positions start to end"
    (are
      [x y thrust angle intermittent-positions]
      (let [actual-entries (custom-math/all-positions-start-to-end-radians x y thrust angle)
            actual (round-positions-vec actual-entries)
            expected (round-positions-vec intermittent-positions)]
        (= expected actual))

      0 0 7 Math/PI
      [(hlt-math/map->Position {:x -1 :y 0})
       (hlt-math/map->Position {:x -2 :y 0})
       (hlt-math/map->Position {:x -3 :y 0})
       (hlt-math/map->Position {:x -4 :y 0})
       (hlt-math/map->Position {:x -5 :y 0})
       (hlt-math/map->Position {:x -6 :y 0})
       (hlt-math/map->Position {:x -7 :y 0})])))
