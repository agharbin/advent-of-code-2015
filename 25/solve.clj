(ns advent.2015.25)

(defn next-code [x]
  (mod (* x 252533N) 33554393N))

(defn triangle [n]
  (/ (* n (inc n)) 2))

(defn grid-position [row col]
  (let [m (- (+ row col) 2)
        n (- col 1)
        l col]
  (+ (- (triangle m) (triangle n)) (triangle l))))

(defn nth-code [n]
  (->> (iterate next-code 20151125N)
       (drop (dec n))
       (take 1)))

(nth-code (grid-position 2947 3029))
