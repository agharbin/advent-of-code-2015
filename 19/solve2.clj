(ns advent.2015.19
  (:require [clojure.string :as s]
            [clojure.pprint :as pp]))

(def rule-regex #"(.*) => (.*)")

(defn parse-input [input]
  (let [raw-rules (re-seq rule-regex input)
        rules (map (comp reverse rest) raw-rules)
        goal (last (s/split-lines input))]
    [rules goal]))

(defn start-end-locations [x s]
  (loop [matcher (re-matcher (re-pattern x) s)
         locations #{}]
    (if (.find matcher)
      (recur matcher (conj locations [(.start matcher) (.end matcher)]))
      locations)))

(defn replace-str [s [start end] v]
  (str (subs s 0 start) v (subs s end)))

(defn neighbors [rules molecule]
  (for [[k v] rules
        replacement-coords (start-end-locations k molecule)]
    (replace-str molecule replacement-coords v)))

(def max-int (Math/pow 2 32))

(defn dfs [rules stack]
  (if (= "e" (peek stack))
    (do
      (printf "Found path of length %d\n" (count (pop stack)))
      (count (pop stack)))
    (loop [xs (neighbors rules (peek stack))
           shortest max-int]
      (if (seq xs)
        (let [candidate (dfs rules (conj stack (first xs)))]
          (recur (rest xs) (min shortest candidate)))
        shortest))))

(defn solve [[rules target-molecule]]
  (dfs rules (list target-molecule)))

(defn solve-file [input]
  (->> input
       slurp
       parse-input
       solve
       pp/pprint))

(solve-file "input.dat")
