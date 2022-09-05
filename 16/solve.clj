(ns advent.2015.16
  (:require [clojure.string :as s]
            [clojure.set :as cset]))

;; Handle Input

(def line-regex #"Sue (\d+): (.*)")
(def keyval-regex #"(\w+): (\d+)")

(defn parse-item [input]
  (let [[_ k v] input]
    [(keyword k) (parse-long v)]))

(defn parse-line [input]
  (let [[_ sue-num-str keyvals] (re-matches line-regex input)
        sue-num (parse-long sue-num-str)]
    (let [matches (re-seq keyval-regex keyvals)]
      (->> matches
        (map parse-item)
        (into {})
        (vector sue-num)))))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)))

;; Logic

(def real-vals 
  {:children    3
   :cats        7
   :samoyeds    2
   :pomeranians 3
   :akitas      0
   :vizslas     0
   :goldfish    5
   :trees       3
   :cars        2
   :perfumes    1})

(def all-keys (into #{} (keys real-vals)))
(def gt-keys #{:cats :trees})
(def lt-keys #{:pomeranians :goldfish})
(def eq-keys (cset/difference all-keys gt-keys lt-keys))

(defn attributes-match [[sue-num attrib-map]]
  (let [present-keys (into #{} (keys attrib-map))
        present-eq-keys (cset/intersection present-keys eq-keys)
        present-gt-keys (cset/intersection present-keys gt-keys)
        present-lt-keys (cset/intersection present-keys lt-keys)]
    (and
      (every? true? (vals (merge-with = (select-keys attrib-map present-eq-keys) 
                                        (select-keys real-vals  present-eq-keys))))
      (every? true? (vals (merge-with < (select-keys attrib-map present-lt-keys)
                                        (select-keys real-vals  present-lt-keys))))
      (every? true? (vals (merge-with > (select-keys attrib-map present-gt-keys)
                                        (select-keys real-vals  present-gt-keys)))))))

(defn solve [input]
  (filter attributes-match input))

(defn solve-file [input]
  (-> input
      slurp
      parse-input
      solve
      prn))

(solve-file "input.dat")
