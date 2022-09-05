(ns advent.2015.15
  (:require [clojure.string :as s]))

;; Input Handling

(def regex #".*: capacity (.*), durability (.*), flavor (.*), texture (.*), calories (.*)")

(defn parse-line [l]
  (let [[_ capacity durability flavor texture calories] (re-matches regex l)]
    [(parse-long capacity)
     (parse-long durability)
     (parse-long flavor)
     (parse-long texture)
     (parse-long calories)]))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)))

;; General Functions

(defn scalar-mult [s v]
  (map * (repeat s) v))

(defn compute-score
  ([matrix]
   (fn [x] (compute-score matrix x)))
  ([matrix measurements]
   (->> (map scalar-mult measurements matrix)
        (apply map +)
        (map (partial max 0))
        (take 4)
        (apply *))))

(defn compute-calories [matrix measurements]
  (->> (map scalar-mult measurements matrix)
       (apply map +)
       (map (partial max 0))
       last))

;; Part 1 Solution: hill-climbing to find local maxima. Try many times to find the
;; best answer.

(defn move-tablespoon
  ([measurements]
   (fn [x] (move-tablespoon measurements x)))
  ([measurements [to from]]
   (-> measurements
       (update to inc)
       (update from dec))))

(defn all-non-negative? [x]
  (every? (complement neg?) x))

(defn nearby-points [measurements]
  (let [size (count measurements)
        index-pairs (for [x (range size) y (range size) :when (not= x y)] [x y])
        neighbor-candidates (map (move-tablespoon measurements) index-pairs)
        neighbors (filter all-non-negative? neighbor-candidates)]
    neighbors))

(defn random-vec [n sum]
  (let [boundaries (sort (for [x (range (dec n))] (rand-int (inc sum))))]
    (vec
      (map - (concat boundaries [sum])
             (concat [0] boundaries)))))

(defn hill-climb [matrix start]
  (loop [current-guess start
         last-score 0]
    (let [neighbors (nearby-points current-guess)
          best-neighbor (apply max-key (compute-score matrix) neighbors)
          best-neighbor-score (compute-score matrix best-neighbor)]
      (if (<= best-neighbor-score last-score)
        [current-guess last-score] ; we've hit a local maximum
        (recur best-neighbor best-neighbor-score)))))

(defn hill-climb-trials [matrix]
  (loop [times 100
         highest-score 0
         highest-point nil]
    (if (= 0 times)
      (println "Done")
      (do
        (println "--Iteration--")
        (println "Highest so far: " highest-score highest-point)
        (let [[neighbor score] (hill-climb matrix (random-vec 4 100))]
          (println "New-Candidate " score neighbor)
          (if (> score highest-score)
              (recur (dec times) score neighbor)
              (recur (dec times) highest-score highest-point)))))))

;; Part 2 Logic
;;
;; Solved system of equations to find that the first 2 rows' weightings must add to 60
;; and the 2nd 2 rows weightings must add to 40. 
;; 8x + 3y = 500
;;  x +  y = 100

(def valid-measurements
  (for [x (range (inc 60))
        y (range (inc 40))] 
      [x (- 60 x) y (- 40 y)]))

(defn solve-2 [matrix]
  (->> valid-measurements
       (map (juxt identity (compute-score matrix)))
       (apply max-key second)))

;; Main Program

(defn solve-file [input]
  (-> input
      slurp
      parse-input
      solve-2
      prn))

(solve-file "input.dat")
