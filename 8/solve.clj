(ns advent.2015.8
  (:require [clojure.string :as cs]))

(defn escaped-length [s]
  (-> s
      (cs/replace #"\\\\" "|")
      (cs/replace #"\\\"" "|")
      (cs/replace #"\\x.." "|")
      count
      dec
      dec))

(defn reverse-escaped-length [s]
  (-> s
      (cs/replace #"(\\|\")" "\\\\$1")
      count
      inc
      inc))

(defn solve [input]
  (->> input
       (map (juxt count reverse-escaped-length))
       (apply map +)
       (apply -)))

(defn solve-file [input]
  (-> input
      slurp
      cs/split-lines
      solve
      prn))

(solve-file "input.dat")
