(ns advent.2015.12
  (:require [clojure.data.json :as json]))

(defn solve [input]
  (cond
    (number? input)
      input
    (string? input)
      0
    (vector? input)
      (apply + (map solve input))
    (and (map? input) (some #{"red"} (vals input)))
      0
    (map? input)
      (+ (apply + (map solve (keys input)))
         (apply + (map solve (vals input))))))

(defn solve-file [input]
  (-> input
      slurp
      json/read-str
      solve
      prn))

(solve-file "input.dat")
