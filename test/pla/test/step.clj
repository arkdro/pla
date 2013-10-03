(ns pla.test.step
  (:use [pla.step])
  (:use [clojure.test]))

(defn inside-interval [n]
  (and
   (<= -1 n)
   (>= 1.00001 n)
   ))

(deftest mk-rand-test
  (let [res (pla.step/mk-rand)]
    (is (inside-interval res)
        )))

(deftest mk-point-test
  (let [[x y] (pla.step/mk-point)]
    (is (inside-interval x))
    (is (inside-interval y))
    ))

(deftest mk-line-test
  (let [[[x1 y1] [x2 y2]] (pla.step/mk-line)]
    (is (inside-interval x1))
    (is (inside-interval y1))
    (is (inside-interval x2))
    (is (inside-interval y2))
    ))

