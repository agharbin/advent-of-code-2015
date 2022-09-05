(ns advent.2015.5
  (:require [clojure.string :as s]))

(def vowels #{\a \e \i \o \u})

(def illegal-pairs #{[\a \b] [\c \d] [\p \q] [\x \y]})

(defn has-three-vowels? [w] (->> w (filter vowels) count (<= 3)))

(defn double-letter? [w] (->> w (partition 2 1) (filter #(= (first %) (second %))) count pos?))

(defn no-illegal-pairs? [w] (->> w (partition 2 1) (filter illegal-pairs) count zero?))

(defn nice-1? [w] (and (has-three-vowels? w) (double-letter? w) (no-illegal-pairs? w)))

(defn double-pair? [w]
  (if (seq w)
    (let [pair (take 2 w)
          remainder (->> w (drop 2) (partition 2 1) (into #{}))]
      (if (contains? remainder pair)
        true
        (recur (rest w))))
    false))

(defn third [x] (nth x 2))

(defn repeated-letter-skipping? [w] (->> w (partition 3 1) (filter #(= (first %) (third %))) count pos?))

(defn nice-2? [w] (and (double-pair? w) (repeated-letter-skipping? w)))

(defn solve [input]
  (->> input
       (filter nice-2?)
       count))

(defn parse-input [input]
  (->> input
       (s/split-lines)))

(defn solve-file [input]
  (-> input
      slurp
      parse-input
      solve
      prn))

(solve-file "input.dat")
