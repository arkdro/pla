(ns pla.test.step
  (:use clojure.tools.trace)
  (:use [pla.step])
  (:use [clojure.test]))

;; (trace-ns 'pla.step)

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

(deftest mk-any-line-test
  (let [[[x1 y1] [x2 y2]] (pla.step/mk-any-line)]
    (is (inside-interval x1))
    (is (inside-interval y1))
    (is (inside-interval x2))
    (is (inside-interval y2))
    ))

(deftest calc-line-w-test
  (is (= [14/3 5/2 -77/6] (pla.step/calc-line-w [[3/2 7/3] [-1 7]])))
  )

(deftest normalize-by-x-test
  (is (= [[-1 7] [1 49/15]] (pla.step/normalize-by-x 14/3 5/2 -77/6)))
  (is (= [[-1 23/7] [1 31/14]] (pla.step/normalize-by-x 5/2 14/3 -77/6)))
  )

(deftest normalize-by-y-test
  (is (= [[23/7 -1] [31/14 1]] (pla.step/normalize-by-y 14/3 5/2 -77/6)))
  (is (= [[7 -1] [49/15 1]] (pla.step/normalize-by-y 5/2 14/3 -77/6)))
  )

(deftest calc-one-y-test
  (is (= 1 (pla.step/calc-one-y [[-1 0] [1 0]] [0 0])))
  (is (= 1 (pla.step/calc-one-y [[-1 0] [1 0]] [1 -0.0001])))
  (is (= -1 (pla.step/calc-one-y [[-1 0] [1 0]] [1 0.0001])))

  (is (= 1 (pla.step/calc-one-y [[0 -1] [0 1]] [0 0])))
  (is (= 1 (pla.step/calc-one-y [[0 -1] [0 1]] [0.0001 0])))
  (is (= -1 (pla.step/calc-one-y [[0 -1] [0 1]] [-0.0001 0])))
  (is (= 1 (pla.step/calc-one-y [[-1 -1] [1 1]] [3 3])))

  (is (= 1 (pla.step/calc-one-y [[-1 -1] [1 1]] [3 0])))
  (is (= 1 (pla.step/calc-one-y [[-1 -1] [1 1]] [0 -1])))
  (is (= -1 (pla.step/calc-one-y [[-1 -1] [1 1]] [0 1])))
  )

(deftest calc-y-test
  (is (= [-1] (pla.step/calc-y [[-1 0] [1 0]] [[3 3]])))
  (is (= [1] (pla.step/calc-y [[-1 -1] [1 1]] [[1 1]])))
  (is (= [1] (pla.step/calc-y [[-1 -1] [1 1]] [[3.01 3]])))
  (is (= [1] (pla.step/calc-y [[-1 0] [1 0]] [[-3 -3]])))
  )

(deftest split-points-test
  (is (= [[] [[3.01 3]]] (pla.step/split-points [1] [[3.01 3]])))
  (is (= [[[0.1 -0.9]] [[3.01 3]]]
         (pla.step/split-points [-1 1] [[0.1 -0.9] [3.01 3]])))
  )

(deftest is-misclassified-test
  (is (= true (pla.step/is-misclassified [0 0 0] [1 [3.01 3]])))
  (is (= true (pla.step/is-misclassified [0 0 0] [-1 [3.01 3]])))
  (is (= true (pla.step/is-misclassified [1 0.5 1.23] [-1 [3.01 3]])))
  (is (= false (pla.step/is-misclassified [1 0.5 1.23] [1 [3.01 3]])))
  )

(deftest calc-one-y2-test
  (is (= 1 (pla.step/calc-one-y2 [1 0.21 0.33] [-1 0])))
  (is (= 1 (pla.step/calc-one-y2 [1 0.25 0.25] [-1 -3])))
  (is (= -1 (pla.step/calc-one-y2 [1 0.21 0.33] [-1 -3])))
  )

(deftest get-misclassified-test
  (is (= [-1 [0.1 -0.9]] (pla.step/get-misclassified [1 0.21 1.13]
                                                     [-1 1]
                                                     [[0.1 -0.9] [3.01 3]]
                                                     )))
  (is (= nil (pla.step/get-misclassified [1 0.21 1.31]
                                         [-1 1]
                                         [[0.1 -0.9] [3.01 3]]
                                         )))
  )

(deftest update-w-test
  (is (= [2 -0.79 -2.67] (pla.step/update-w [1 0.21 0.33] 1 [-1 -3])))
  (is (= [1 -0.79 3.33] (pla.step/update-w [2 0.21 0.33] -1 [1 -3])))
  )

(deftest line-outside-test
  (is (= false (pla.step/line-outside [[-1 -1] [1 1]])))
  (is (= true (pla.step/line-outside [[-1 -1] [-1 1]])))
  (is (= false (pla.step/line-outside [[0.21 0.33] [-1 -3]])))
  (is (= false (pla.step/line-outside [[0.21 0.33] [-0.1 -0.3]])))
  (is (= true (pla.step/line-outside [[-1.1 -1] [-1 1.1]])))
  )

;; this is not a test, actually
(deftest pla-test
  (let [
        line [[-1 0.34843307003649615] [1 1.9429805981704393]]
        points [[-0.4855611243127945 -0.7666774445881908]
                [-0.37618070384831104 -0.8997041076731649]
                [0.6562998659034687 -0.011240559143512074]
                [0.7630545553687544 -0.45373263005814524]
                [0.8186682345709249 -0.47598280906682655]
                [-0.38335037355545865 -0.38137643987710845]
                [0.46756972608225356 0.9827115899665597]
                [0.3745667278252942 -0.8720781000939429]
                [0.1876235444072898 0.9160007479127457]
                [0.6606382312174957 -0.7996944466429889]]
        ys (pla.step/calc-y line points)
        _ (is (= ys [1 1 1 1 1 1 1 1 1 1]))
        init-w [0 0 0]
        act (pla.step/pla init-w ys points)
        _ (pla.step/plot-one-square line points [] "pla-test-2")
        ]
    (println "pla-test, act" act)
    )
  )
