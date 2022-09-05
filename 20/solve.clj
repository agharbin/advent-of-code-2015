(ns advent.2015.20)

(defn divides [x y]
  (= 0 (mod y x)))

(defn factors [x]
  (let [sqrt (Math/ceil (Math/sqrt x))]
    (into #{} (flatten (for [i (range 1 (inc sqrt)) :when (divides i x)] [i (/ x i)])))))

(defn filtered-factors [x]
  (let [xs (factors x)]
    (filter #(<= x (* 50 %))  xs)))

(defn score [x] (* 11 (apply + (filtered-factors x))))

(defn solve [input]
  (->> (range)
       (drop 1)
       (drop-while #(< (score %) input))
       first))

  (solve 34000000)
