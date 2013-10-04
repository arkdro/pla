(ns pla.step
  ;; (:use [incanter core stats charts])
  )

(defn plot []
  ;; ex1:
  ;; (doto (xy-plot [1 2 3] [4 5 6]) (add-points [1 2 3] [4.1 5.1 6.1]) (set-stroke-color java.awt.Color/black :series 0) (set-stroke-color java.awt.Color/red :series 1) view)

  ;; ex2:
  ;; (def c1 (xy-plot [-1 0.65] [-0.3 0.8])) ;; this is the first line
  ;; (set-x-range c1 -1 1)
  ;; (set-y-range c1 -1 1)
  ;; (set-stroke c1 :width 4 :dash 5)
  ;; (set-stroke-color c1 java.awt.Color/red :series 0) ;; for first line
  ;; (set-stroke-color c1 java.awt.Color/red :series 1) ;; for second dot
  ;; (set-stroke-color c1 java.awt.Color/red :series 2) ;; for third dot pack
  ;; (add-points c1 [0.88] [0.01]) ;; this is the second dot
  ;; (add-points c1 [0.15 0.55 0.95] [3.15 2.91 2.51]) ;; this is the 3rd pack
  ;; (save c1 "/tmp/c1-1.png" :width 600 :height 500)
  ;; (view c1)
  )

(defn mk-rand []
  (- (rand 2.000001) 1))

(defn mk-point []
  [(mk-rand)
   (mk-rand)]
  )

(defn mk-line []
  [(mk-point)
   (mk-point)]
  )

(defn calc-line-w [ [[x1 y1 :as p1] [x2 y2 :as p2]] ]
  (let [dy (- y2 y1)
        dx (- x2 x1)
        cx (* dy (- x1))
        cy (* dx (- y1))
        c (- cx cy)]
    [dy (- dx) c]))

(defn normalize-by-x [kx ky c]
  (let [
        x1 -1
        y1 (/ (- kx c) ky)
        x2 1
        y2 (/ (- (+ kx c)) ky)
        ]
    [[x1 y1] [x2 y2]]
    )
  )

(defn normalize-by-y [kx ky c]
  (let [
        y1 -1
        x1 (/ (- ky c) kx)
        y2 1
        x2 (/ (- (+ ky c)) kx)
        ]
    [[x1 y1] [x2 y2]]
    )
  )

;; given line coefficients, return two points located far enough from
;; each other
(defn normalize [kx ky c]
  (if (< (pla.misc/abs kx) (pla.misc/abs ky))
    (normalize-by-x kx ky c)
    (normalize-by-y kx ky c)))

(defn mk-norm-line []
  (let [line (mk-line)
        [kx ky c] (calc-line-w line)]
    (normalize kx ky c)))

(defn calc [n]
  (println "n: " n)
  )

