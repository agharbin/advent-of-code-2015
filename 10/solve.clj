(ns advent.2015.10)

(defn next-step [input]
  (->> input
       (partition-by identity)
       (mapcat (juxt count first))
       (apply str)))

(def input "3113322113")
(def times 50)

(count (nth (iterate next-step input) times))
