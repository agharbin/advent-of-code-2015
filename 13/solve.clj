(ns advent.2015.13
  (:require [clojure.string :as s]
            [clojure.math.combinatorics :refer [permutations]]))

(def regex #"(.*) would (gain|lose) (.*) happiness units by sitting next to (.*)\.")

(defn parse-line [line]
  (let [[_ name1 gain-lose raw-value name2] (re-matches regex line)
        value (parse-long raw-value)
        signed-value (if (= "gain" gain-lose) value (* -1 value))]
      [[(keyword name1) (keyword name2)] signed-value]))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)
       (into {})))

(defn score-permutation [m xs]
  (loop [pairs (partition 2 1 xs)
         score 0]
    (if (seq pairs)
      (let [[p1 p2] (first pairs)]
        (recur (rest pairs) (+ score
                               (m [p1 p2] 0)
                               (m [p2 p1] 0))))
      ; Since we are treating the list as circular, we must
      ; add the scores of the first and last people since
      ; they are sitting adjacent to each other.
      (let [p1 (first xs)
            p2 (last xs)]
        (+ score
           (m [p1 p2] 0)
           (m [p2 p1] 0))))))

(defn solve [input]
  (let [people (->> input keys (apply concat) (into #{}))
        possible-seatings (permutations people)]
    (->> possible-seatings
         (map (partial score-permutation input))
         (apply max))))

(defn solve-file [input]
  (-> input
      slurp
      parse-input
      solve
      prn))

(solve-file "input.dat")
