(ns advent.2015.1)

(defn solve [input]
  (loop [input input
         floor 0
         step 0]
    (if (< floor 0)
        step
        (case (first input)
          \( (recur (rest input) (inc floor) (inc step))
          \) (recur (rest input) (dec floor) (inc step))))))

(defn parse-input [input]
  input)

(defn solve-file [input]
  (-> input
      slurp
      parse-input
      solve))

(solve-file "input.dat")
