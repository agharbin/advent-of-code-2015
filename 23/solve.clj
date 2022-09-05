(ns advent.2015.23
  (:require [clojure.string :as s]
            [clojure.pprint :as pp]
            [clojure.core.match :as m]))

;; Input Handling

(def input-regex #"(\w+) ([a-b0-9-+]+)(?:, )?([0-9-+]+)?")

(defn parse-line [input]
  (let [[opcode arg1 arg2] (rest (re-matches input-regex input))]
    (m/match [opcode arg1 arg2]
      ["hlf" r _] [:hlf (keyword r)]
      ["tpl" r _] [:tpl (keyword r)]
      ["inc" r _] [:inc (keyword r)]
      ["jmp" o _] [:jmp (parse-long o)]
      ["jie" r o] [:jie (keyword r) (parse-long o)]
      ["jio" r o] [:jio (keyword r) (parse-long o)]
      :else       "Error")))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map parse-line)
       (apply vector)))

;; Logic

(defn half [x] (/ x 2))

(defn triple [x] (* x 3))

(defn compute-registers [program]
  (loop [registers {:a 0 :b 0}
         read-loc  0]
    (if (contains? program read-loc)
      (let [instruction (program read-loc)]
        (m/match instruction
          [:hlf r]   (recur (update-in registers [r] half)
                            (inc read-loc))
          [:tpl r]   (recur (update-in registers [r] triple)
                            (inc read-loc))
          [:inc r]   (recur (update-in registers [r] inc)
                            (inc read-loc))
          [:jmp o]   (recur registers
                            (+ read-loc o))
          [:jie r o] (recur registers
                            (if (even? (registers r)) (+ read-loc o) (inc read-loc)))
          [:jio r o] (recur registers
                            (if (= 1 (registers r)) (+ read-loc o) (inc read-loc)))))
      registers)))

(defn solve-file [input]
  (-> input
      slurp
      parse-input
      compute-registers
      pp/pprint))

(solve-file "input.dat")
