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

(defn call-calc [n cnt verbose pic]
  (if verbose
    (binding [*out* *err* pla.misc/*verbose* 'true]
      (time (pla.step/calc n cnt pic)))
    (pla.step/calc n cnt pic)))

(defn print-result [res]
  (println "res: " res))

(defn -main [& args]
  (let [opts (cli
              args
              ["-v" "--[no-]verbose" :default false]
              ["-p" "--[no-]picture" :default false]
              ["-c" "--cnt" "Count of experiments"
               :parse-fn #(Integer. %)
               :default 1]
              ["-n" "--n" "N" :parse-fn #(Integer. %)])
        [options _ _] opts
        res (call-calc (:n options)
                       (:cnt options)
                       (:verbose options)
                       (:picture options))
        ]
    (print-result res)))

