(ns advent.2015.21)

(def boss {:hp 109 :dm 8 :ar 2})

(def player {:hp 100})

(def weapons [{:cost 8  :dm 4 :ar 0}
              {:cost 10 :dm 5 :ar 0}
              {:cost 25 :dm 6 :ar 0}
              {:cost 40 :dm 7 :ar 0}
              {:cost 74 :dm 8 :ar 0}])

(def armor [{:cost 0 :dm 0 :ar 0}
            {:cost 13 :dm 0 :ar 1}
            {:cost 31 :dm 0 :ar 2}
            {:cost 53 :dm 0 :ar 3}
            {:cost 75 :dm 0 :ar 4}
            {:cost 102 :dm 0 :ar 5}])

(def rings [{:cost 0 :dm 0 :ar 0 :dummy 0}
            {:cost 0 :dm 0 :ar 0}
            {:cost 25 :dm 1 :ar 0}
            {:cost 50 :dm 2 :ar 0}
            {:cost 100 :dm 3 :ar 0}
            {:cost 20 :dm 0 :ar 1}
            {:cost 40 :dm 0 :ar 2}
            {:cost 80 :dm 0 :ar 3}])

(defn player-stats [weapon armor ring-1 ring-2]
  (merge-with + player weapon armor ring-1 ring-2))

(defn damage [source target]
  (max 1
       (- (source :dm) (target :ar))))

(defn player-wins? [player]
  (loop [turn :player
         player-hp (:hp player)
         boss-hp (:hp boss)]
    (case turn
      :player (if (<= (- boss-hp (damage player boss)) 0)
                true
                (recur :boss player-hp (- boss-hp (damage player boss))))
      :boss (if (<= (- player-hp (damage boss player)) 0)
                false
                (recur :player (- player-hp (damage boss player)) boss-hp)))))

(let [combinations
        (for [w weapons
              a armor
              r1 rings
              r2 rings
              :when (not= r1 r2)]
          (player-stats w a r1 r2))]
  (->> combinations
       (filter (complement player-wins?))
       (map :cost)
       (apply max)))
