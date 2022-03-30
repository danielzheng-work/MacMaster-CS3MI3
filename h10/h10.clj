

(defn summingPairs [xs sum]
    (letfn [(summingPairsHelper [xs ys the_pairs]
        (if (empty? xs) the_pairs
            (let [[fst & rest] xs]
              (recur
               rest ys
               (doall 
                (concat the_pairs
                        (for [snd ys 
                              :when (<= (+ fst snd) sum)]
                          [fst snd])))))))]
    ;; split the list into two sublist then use them concurrently
      (let [number (count xs)](
        let [left (quot number 2)
            right   (- number (quot number 2))]
            (let [left-item (take left xs)
                right-item (take-last right xs)]
                (let [list1  (future (summingPairsHelper xs left-item []))
                list2(future (summingPairsHelper xs right-item []))]
                (concat @list1 @list2)))))))





