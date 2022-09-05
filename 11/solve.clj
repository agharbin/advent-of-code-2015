(ns advent.2015.11
  (:require [clojure.string :as st]))

(def full-alphabet (seq "abcdefghijklmnopqrstuvwxyz"))
(def illegal-chars #{\i \o \l})
(def legal-chars (filter (complement illegal-chars) full-alphabet))
(def legal-doubles (map #(str % %) legal-chars))
(def doubles-regex (re-pattern (str "(.*)(" (st/join "|" legal-doubles) ")(.*)")))
(def legal-triplets (->> full-alphabet
                         (partition 3 1)
                         (filter #(not-any? illegal-chars %))
                         (map #(apply str %))))
(def triples-regex (re-pattern (st/join "|" legal-triplets)))

(defn has-two-doubles? [s]
  (boolean
    (if-let [[_ prefix _ postfix] (re-matches doubles-regex s)]
      (or
        (re-matches doubles-regex prefix)
        (re-matches doubles-regex postfix)))))

(defn is-legal? [s]
  (boolean
    (and (re-find triples-regex s)
         (not-any? illegal-chars s)
         (has-two-doubles? s))))

(defn next-char [c]
  (char (inc (int c))))

(defn increment-string [s]
  (let [reversed (reverse s)
        starting-zs (take-while #{\z} reversed)
        num-starting-zs (count starting-zs)
        remaining-chars (drop-while #{\z} reversed)]
    (apply str
      (reverse
        (concat (apply str (take num-starting-zs (repeat \a)))
                (list (next-char (first remaining-chars)))
                (rest remaining-chars))))))

(defn next-valid-string [s]
  (first (filter is-legal? (iterate increment-string (increment-string s)))))

(next-valid-string "hxbxxyzz")
