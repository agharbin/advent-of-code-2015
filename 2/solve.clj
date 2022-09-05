(ns advent.2015.2
  (:require [clojure.string :as s]))

(defn compute-ribbon [[x y z :as dimensions]]
  (let [smallest-2-dimensions (->> dimensions sort (take 2))
        smallest-perimeter (->> smallest-2-dimensions (map #(* 2 %)) (apply +))
        cubic-volume (reduce * dimensions)]
    (+ smallest-perimeter cubic-volume)))

(defn compute-paper [[x y z]]
  (let [side-1 (* x y)
        side-2 (* x z)
        side-3 (* y z)
        min-side (min side-1 side-2 side-3)
        total-paper (+ min-side (* 2 side-1) (* 2 side-2) (* 2 side-3))]
    total-paper))

(defn solve [input]
  (->> input
       (map compute-ribbon)
       (apply +)))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map #(s/split % #"x"))
       (map #(map parse-long %))))

(defn solve-file [input]
  (-> input
      slurp
      parse-input
      solve
      prn))

(solve-file "input.dat")
