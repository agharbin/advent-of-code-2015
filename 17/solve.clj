(ns advent.2015.17
  (:require [clojure.string :as s]))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-long)))

(def solve
  (memoize
    (fn [liters containers]
      (cond
        (and (empty? containers) (= 0 liters))
          ['()] 
        (and (empty? containers) (not= 0 liters))
          []
        :else
          (let [container (first containers)
                suffixes-with-container (solve (- liters container) (rest containers))
                suffixes-without-container (solve liters (rest containers))]
            (concat
              (map #(conj % container) suffixes-with-container)
              suffixes-without-container))))))

(defn solve-file [input]
  (->> input
       slurp
       parse-input
       (solve 150)
       (map count)
       frequencies
       prn))

(solve-file "input.dat")
