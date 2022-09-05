(ns advent.2015.19
  (:require [clojure.string :as s]
            [clojure.pprint :as pp]))

(def rule-regex #"(.*) => (.*)")

(defn parse-input [input]
  (let [raw-rules (re-seq rule-regex input)
        rules (map rest raw-rules)
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

(defn solve [[rules molecule]]
  (for [[k v] rules
        replacement-coords (start-end-locations k molecule)]
    (replace-str molecule replacement-coords v)))

(defn solve-file [input]
  (->> input
       slurp
       parse-input
       solve
       (into #{})
       count
       pp/pprint))

(solve-file "input.dat")
