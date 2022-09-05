(ns advent.2015.6
  (:require [clojure.string :as s]))

(defn parse-instruction [state x1 y1 x2 y2]
  (let [state-kw (case state
                   "turn off" :off
                   "turn on"  :on
                   "toggle"   :toggle)]
    [state-kw [(parse-long x1) (parse-long y1) (parse-long x2) (parse-long y2)]]))

(defn parse-input [input]
  (->> input
       (s/split-lines)
       (map #(re-matches #"(turn off|turn on|toggle) (\d+),(\d+) through (\d+),(\d+)" %))
       (map rest)
       (map (partial apply parse-instruction))))

(defn coords [[xmin ymin xmax ymax]]
  (for [x (range xmin (inc xmax)) y (range ymin (inc ymax))] [x y]))

(defn execute-instruction [grid instruction]
  (prn "executing" instruction)
  (let [[command square] instruction
        update-func (case command
                      :on inc
                      :off (fn [x] (max 0 (dec x)))
                      :toggle (fn [x] (+ 2 x)))
        coords-to-update (coords square)]
    (loop [coords coords-to-update
           grid grid]
      (if (seq coords)
        (recur (rest coords) (update grid (first coords) update-func))
        grid))))

(defn solve [input]
  (let [grid (into {} (for [x (range 1000) y (range 1000)] [[x y] 0]))]
    (reduce execute-instruction grid input)))

(defn solve-file [input]
  (->> input
       slurp
       parse-input
       solve
       (map second)
       (apply +)
       prn))

(solve-file "input.dat")
