(ns advent.2015.18
  (:require [clojure.string :as s]))

;; Handle Input

(defn seq2d->vec2d [x]
  (vec (map vec x)))

(defn parse-input [input]
  (->> input
       s/split-lines
       seq2d->vec2d))

;; Logic

(defn nearby-coords [[r c]]
  (for [r-increment [-1 0 1]
        c-increment [-1 0 1]
        :when (not (and (= r-increment 0) (= c-increment 0)))]
    [(+ r r-increment) (+ c c-increment)]))

(defn valid-coord?
  ([row-col-sizes]
   (fn [x] (valid-coord? row-col-sizes x)))
  ([[num-rows num-cols] [r c]]
   (let [bottom-edge (dec num-rows)
         right-edge (dec num-cols)]
     (and (<= 0 r bottom-edge)
          (<= 0 c right-edge)))))

(defn valid-neighbors [row-col-sizes coord]
  (filter (valid-coord? row-col-sizes) (nearby-coords coord)))

(defn grid-dimensions [grid]
  (let [num-rows (count grid)
        num-cols (count (first grid))]
    [num-rows num-cols]))

(defn nearby-lights [grid coords]
  (let [neighbor-coords (valid-neighbors (grid-dimensions grid) coords)
        neighbor-states (map (partial get-in grid) neighbor-coords)
        neighbor-lights (filter #{\#} neighbor-states)]
       (count neighbor-lights)))

(defn next-light-state [current-state num-adjacent-lights]
  (if (= current-state \#)
    (if (<= 2 num-adjacent-lights 3) \# \.)
    (if (= num-adjacent-lights 3) \# \.)))

(defn const-on [& _] \#)

(defn corners-on [grid]
  (let [[num-rows num-cols] (grid-dimensions grid)
        right-edge (dec num-cols)
        bottom-edge (dec num-rows)]
    (-> grid
        (update-in [0 0]                    const-on)
        (update-in [0 right-edge]           const-on)
        (update-in [bottom-edge 0]          const-on)
        (update-in [bottom-edge right-edge] const-on))))

(defn next-grid [grid]
  (let [[num-rows num-cols] (grid-dimensions grid)
        next-values (for [r (range num-rows)]
                      (for [c (range num-cols)]
                        (next-light-state (get-in grid [r c])
                                          (nearby-lights grid [r c]))))]
    (->> next-values
         seq2d->vec2d
         corners-on)))

(defn gen-solutions [grid]
  (iterate next-grid grid))

(def iterations 4)

(defn solve-file [input]
  (let [initial-grid (-> input slurp parse-input corners-on)
        solution-seq (gen-solutions initial-grid)
        solution (->> solution-seq (drop 1) (take iterations) last)
        final-count (->> solution (apply concat) (filter #{\#}) count)]
    final-count))

(solve-file "sample.dat")
