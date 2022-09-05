(ns advent.2015.22)

(def player {:hp 50 :mp 500})
(def boss {:hp 51 :dm 9})

(def initial-state
  {:player player
   :boss boss
   :mana 0
   :shield-turns 0
   :poison-turns 0
   :recharge-turns 0})

;; Status Effects
(defn recharge [state]
  (if (< 0 (:recharge-turns state))
    (-> state
        (update-in [:player :mp] #(+ 101 %))
        (update-in [:recharge-turns] dec))
    state))

(defn poison [state]
  (if (< 0 (:poison-turns state))
    (-> state
        (update-in [:boss :hp] #(- % 3))
        (update-in [:poison-turns] dec))
    state))

(defn shield [state]
  (if (< 0 (:shield-turns state))
    (-> state
        (update-in [:shield-turns] dec))
    state))

(defn lose-hp [state]
  (update-in state [:player :hp] dec))

;; Spells
(defn cast-magic-missile [state]
  (-> state
      (update-in [:player :mp] #(- % 53))
      (update-in [:mana] #(+ % 53))
      (update-in [:boss :hp] #(- % 4))))

(defn cast-drain [state]
  (-> state
      (update-in [:player :mp] #(- % 73))
      (update-in [:mana] #(+ % 73))
      (update-in [:boss :hp] #(- % 2))
      (update-in [:player :hp] #(+ % 2))))

(defn cast-shield [state]
  (-> state
      (update-in [:player :mp] #(- % 113))
      (update-in [:mana] #(+ % 113))
      (assoc-in [:shield-turns] 7)))

(defn cast-poison [state]
  (-> state
      (update-in [:player :mp] #(- % 173))
      (update-in [:mana] #(+ % 173))
      (assoc-in [:poison-turns] 6)))

(defn cast-recharge [state]
  (-> state
      (update-in [:player :mp] #(- % 229))
      (update-in [:mana] #(+ % 229))
      (assoc-in [:recharge-turns] 5)))

(defn available-spells [state]
  (let [mp (get-in state [:player :mp])]
    (cond-> []
      (<= 53 mp)  (conj 'cast-magic-missile)
      (<= 73 mp)  (conj 'cast-drain)
      (and (<= 113 mp) (= 0 (:shield-turns state))) (conj 'cast-shield)
      (and (<= 173 mp) (= 0 (:poison-turns state))) (conj 'cast-poison)
      (and (<= 229 mp) (= 0 (:recharge-turns state))) (conj 'cast-recharge))))

(def max-mana 99999)

(defn mana-used [state phase]
  (cond
    (< max-mana (:mana state)) max-mana
    (player-wins? state) (:mana state)
    (player-loses? state) max-mana
    :else (case phase
            1 (recur (-> state poison recharge shield lose-hp) 2)
            2 (loop [spells (available-spells state)
                     min-mana max-mana]
                (if (seq spells)
                  (let [spell (first spells)
                        m (mana-used (@(resolve spell) state) 3)]
                    (recur (rest spells) (min min-mana m)))
                  min-mana))
            3 (recur (-> state poison recharge shield) 4)
            4 (recur (-> state boss-turn) 1))))

;; Actions
(defn boss-turn [state]
  (let [raw-damage (get-in state [:boss :dm])
        effective-damage (if (< 0 (:shield-turns state)) (max 1 (- raw-damage 7)) raw-damage)]
  (-> state
      (update-in [:player :hp] #(- % effective-damage)))))

(defn player-wins? [state]
  (<= (get-in state [:boss :hp]) 0))

(defn player-loses? [state]
  (<= (get-in state [:player :hp]) 0))

(mana-used initial-state 1)
