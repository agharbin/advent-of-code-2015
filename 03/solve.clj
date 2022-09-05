(ns advent.2015.3
  (:require [clojure.string :as s]))

(defn solve [input]
  (loop [input input
         [x y :as position] [0 0]
         visited #{[0 0]}]
    (if (seq input)
      (case (first input)
        \^ (recur (rest input) [x (inc y)] (conj visited [x (inc y)]))
        \v (recur (rest input) [x (dec y)] (conj visited [x (dec y)]))
        \< (recur (rest input) [(dec x) y] (conj visited [(dec x) y]))
        \> (recur (rest input) [(inc x) y] (conj visited [(inc x) y])))
      (count visited))))

(defn solve2 [input]
  (loop [input input
         [x1 y1 :as santas-pos] [0 0]
         [x2 y2 :as robot-pos] [0 0]
         visited #{[0 0]}
         santas-turn true]
    (if (seq input)
      (let [dir (first input)
            robots-turn (not santas-turn)]
        (cond
          (and (= dir \^) santas-turn)
            (recur (rest input) [x1 (inc y1)] robot-pos (conj visited [x1 (inc y1)]) (not santas-turn))
          (and (= dir \v) santas-turn)
            (recur (rest input) [x1 (dec y1)] robot-pos (conj visited [x1 (dec y1)]) (not santas-turn))
          (and (= dir \<) santas-turn)
            (recur (rest input) [(dec x1) y1] robot-pos (conj visited [(dec x1) y1]) (not santas-turn))
          (and (= dir \>) santas-turn)
            (recur (rest input) [(inc x1) y1] robot-pos (conj visited [(inc x1) y1]) (not santas-turn))
          (and (= dir \^) robots-turn)
            (recur (rest input) santas-pos [x2 (inc y2)] (conj visited [x2 (inc y2)]) (not santas-turn))
          (and (= dir \v) robots-turn)
            (recur (rest input) santas-pos [x2 (dec y2)] (conj visited [x2 (dec y2)]) (not santas-turn))
          (and (= dir \<) robots-turn)
            (recur (rest input) santas-pos [(dec x2) y2] (conj visited [(dec x2) y2]) (not santas-turn))
          (and (= dir \>) robots-turn)
            (recur (rest input) santas-pos [(inc x2) y2] (conj visited [(inc x2) y2]) (not santas-turn))))
      (count visited))))

(defn solve-file [input]
  (-> input
      slurp
      (s/trim)
      solve2
      prn))

(solve-file "input.dat")
