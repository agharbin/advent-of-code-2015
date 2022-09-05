(ns advent.2015.14
  (:require [clojure.string :as s]
            [clojure.pprint :as pp]))

;; Input Handling

(def regex #"(.*) can fly (.*) km/s for (.*) seconds, but then must rest for (.*) seconds.")

(defn parse-line [input]
  (let [[_ name speed travel-time rest-time] (re-matches regex input)]
    [name (parse-long speed) (parse-long travel-time) (parse-long rest-time)]))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)))

;; Logic

(defn compute-distance-travelled [time-allowed [deer speed travel-time rest-time]]
  (let [cadence (+ travel-time rest-time)
        total-circuits (quot time-allowed cadence)
        seconds-into-last-circuit (rem time-allowed cadence)
        last-sprint-dist (min (* speed travel-time)
                              (* speed seconds-into-last-circuit))]
  (+ (* speed travel-time total-circuits) last-sprint-dist)))

(defn get-winning-reindeer [deer time]
  (let [deer-rankings (->> deer ; Tuples of [name speed travel-time rest-time]
                           (map #(hash-map :name           (first %)
                                           :dist-travelled (compute-distance-travelled time %)))
                           (sort-by :dist-travelled) ; sorts ascending
                           reverse)
        winning-dist (:dist-travelled (first deer-rankings))
        winning-deer (take-while #(= winning-dist (:dist-travelled %)) deer-rankings)]
    (map :name winning-deer)))

(defn solve [deer]
  (->> (range 1 2505)
       (mapcat #(get-winning-reindeer deer %))
       (frequencies)))

(defn solve-file [input]
  (-> input
      slurp
      parse-input
      solve
      pp/pprint))

(solve-file "input.dat")
