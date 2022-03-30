(defrecord GuardedCommand [guard command])
;;Part 1
(defn allowed-commands [commands]
  (let [result []]
  (if (empty? commands) result
      (let [[command & rest] commands]
        (if (eval (.guard command)) (conj (allowed-commands rest) (.command command))
            (allowed-commands rest))))))

;;Part 2
(defmacro guarded-if
  [& commands]
  `(rand-nth 
   (allowed-commands 
     [~@commands]))) 

;;Part 3
(defmacro guarded-do
  [& commands]
  `(let [command (rand-nth [~@commands])]
        (if (eval (.guard command)) (.command command))
        (if (eval (.guard command)) (guarded-do commands))
        (if (not eval (.guard command)) nil)
            )) 

;;Part 4
(defn gcd [x y]
   (defn findnum [x y]
        (if (zero? y) x
          (recur y (mod x y))))
     (guarded-if
      (GuardedCommand. `(= (mod ~x ~y) 0) (min x y))
      (GuardedCommand. `(= (mod ~y ~x) 0) (min x y))
      (GuardedCommand. `(> ~x ~y) (findnum x y))
      (GuardedCommand. `(< ~x ~y) (findnum y x))))