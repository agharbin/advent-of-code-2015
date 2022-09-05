(ns advent.2015.24
  (:require [clojure.string :as s]))

(def max-int 999999999999) ; Arbitrary integer larger than known best solution

(defn dfs [target xs]
  ((fn dfs-helper [selected sum position]
    (cond
        (= position (count xs)) max-int   ; Failed, off end of input array
        (< target sum) max-int            ; Failed, number too large
        (= target sum) (apply * selected) ; Success
        :else (let [curr-num (xs position)
                    next-pos (inc position)
                    with-curr (dfs-helper (conj selected curr-num) (+ sum curr-num) next-pos)
                    without-curr (dfs-helper selected sum (inc position))]
                (min with-curr without-curr))))
   [] 0 0))

(defn solve [reverse-sorted-input]
  (let [total-weight (apply + reverse-sorted-input)
        group-weight (/ total-weight 4)
        input-with-sentinel (conj reverse-sorted-input 0)]
    (dfs group-weight input-with-sentinel)))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map bigint)
       sort
       reverse
       (apply vector)))

(defn solve-file [input]
  (-> input
      slurp
      parse-input
      solve
      print))

(solve-file "input.dat")
