(ns pla.core
  {:doc "pla"}
  (:use [clojure.tools.cli :only [cli]])
  (:require clojure.string)
  (:use clojure.tools.trace)
  (:require pla.misc)
  (:require pla.step)
  (:gen-class)
  )

;; (trace-ns 'pla.step)

(defn call-calc [n verbose pic]
  (if verbose
    (binding [*out* *err* pla.misc/*verbose* 'true]
      (time (pla.step/calc n pic)))
    (pla.step/calc n pic)))

(defn print-result [res]
  (println "res: " res))

(defn -main [& args]
  (let [opts (cli
              args
              ["-v" "--[no-]verbose" :default false]
              ["-p" "--[no-]picture" :default false]
              ["-n" "--n" "N" :parse-fn #(Integer. %)])
        [options _ _] opts
        res (call-calc (:n options) (:verbose options) (:picture options))
        ]
    (print-result res)))

