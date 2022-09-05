;;; The main component of this solution is to convert an expression into a rule that has 3 components:
;;; (1) a keyword to indicate the wire resulting from the expression (will become the key in a map),
;;; (2) a list of keywords indicating the 'wires' that this wire depends on for its values, and
;;; (3) a function that takes the values of the dependent wires and yields the value of the result wire.
;;; The solve function is just a DFS starting from the goal wire through this graph. It needs to be
;;; memoized to run efficiently.

(ns advent.2015.7
  (:require [clojure.string :as s]
            [clojure.core.match :refer [match]]))

(def m-solve
  "Solve the graph by traversing with DFS. Memoization necessary to avoid very long runtimes."
  (memoize
    (fn [input i]
      (let [[f deps] (i input)
           solved-deps (map #(m-solve input %) deps)]
         (apply f solved-deps)))))

(defn solve [input] (m-solve input :a))

(defn uint-string? [x] (re-matches #"[0-9]+" x))

(defn parse-item [i]
  "Convert a rule into a dictionary entry containing the upstream wires and function for generating the final value from the upstream wires."
  (match i
    [(a :guard uint-string?) "->" r]         [(keyword r) [(fn [] (parse-long a)) []]]
    [a "->" r]                               [(keyword r) [identity [(keyword a)]]]
    ["NOT" (a :guard uint-string?) "->" r]   [(keyword r) [(fn [] (bit-not (parse-long a))) []]]
    ["NOT" a "->" r]                         [(keyword r) [bit-not [(keyword a)]]]
    [(a :guard uint-string?) "OR" b "->" r]  [(keyword r) [#(bit-or % (parse-long a)) [(keyword b)]]]
    [a "OR" b "->" r]                        [(keyword r) [bit-or [(keyword a) (keyword b)]]]
    [(a :guard uint-string?) "AND" b "->" r] [(keyword r) [#(bit-and % (parse-long a)) [(keyword b)]]]
    [a "AND" b "->" r]                       [(keyword r) [bit-and [(keyword a) (keyword b)]]]
    [a "LSHIFT" b "->" r]                    [(keyword r) [#(bit-shift-left % (parse-long b)) [(keyword a)]]]
    [a "RSHIFT" b "->" r]                    [(keyword r) [#(bit-shift-right % (parse-long b)) [(keyword a)]]]
    _                                        :failure))

(defn parse-input [input]
  "Convert input file into dictionary representing the graph of wires and their dependencies."
  (->> input
       (s/split-lines)
       (map #(s/split % #" "))
       (map parse-item)
       (into {})))

(defn unsigned [x] (bit-and x 0xffff)) ; Java does not have unsigned int types

(defn solve-file [input]
  (-> input
      slurp
      parse-input
      solve
      unsigned
      prn))

(solve-file "input2.dat")
