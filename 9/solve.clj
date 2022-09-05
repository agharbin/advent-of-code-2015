(ns advent.2015.9
  (:require [clojure.string :as s]
            [clojure.set :as cset]
            [clojure.math.combinatorics :refer [permutations]]))

(defn calc-path-length [locations-to-dist path]
  (->> path (partition 2 1) (map set) (map locations-to-dist) (apply +)))

(defn solve [locations-to-dist]
  (let [locations (apply cset/union (keys locations-to-dist))
        possible-paths (permutations locations)
        path-lengths (map calc-path-length (repeat locations-to-dist) possible-paths)]
    (apply min path-lengths)))

(defn parse-line [s]
  (let [[_ src dst dist] (re-matches #"(.*) to (.*) = (.*)" s)]
    [#{src dst} (parse-long dist)]))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)))

(defn solve-file [input]
  (->> input
       slurp
       parse-input
       (into {})
       solve
       print))

(solve-file "input.dat")
