(defrecord GCConst [integer])
(defrecord GCVar [symbol])
(defrecord GCOp [expr1 expr2 operation])
(defrecord GCComp [expr1 expr2 compare])
(defrecord GCAnd [test1, test2])
(defrecord GCOr [test1, test2])
(defrecord GCTrue [])
(defrecord GCFalse [])
(defrecord GCSkip [])
(defrecord GCAssign [symbol expr])
(defrecord GCComp [stat1 stat2])
(defrecord GCIf [list])
(defrecord GCDo [list])
(defrecord Config [stmt sig])
(defn emptyState [] 0)
(defn updateState [sig var n]
    (fn [v]
        (if (= v var) n
        (sig v))))

(defn reduce [config]
    (cond
        (= (type (.stmt config)) GCConst) (.integer (.stmt config))
        (= (type (.stmt config)) GCVar) ((.sig config) (.symbol (.stmt config)))
        (= (type (.stmt config)) GCOp) ( let 
            [value1 (reduce (Config. (.expr1 (.stmt config)) (.sig config))) 
            value2 (reduce (Config. (.expr2 (.stmt config)) (.sig config)))] 
                ((.operation (.stmt config)) value1 value2))
        
        (= (type (.stmt config)) GCComp) ( let 
            [value1 (reduce (Config. (.expr1 (.stmt config)) (.sig config))) 
            value2 (reduce (Config. (.expr2 (.stmt config)) (.sig config)))] 
                ((.compare (.stmt config)) value1 value2))
        (= (type (.stmt config)) GCAnd) ( let 
            [t1 (reduce (Config. (.test1 (.stmt config)) (.sig config))) 
            t2 (reduce (Config. (.test2 (.stmt config)) (.sig config)))] 
                (and t1 t2))
          
        (= (type (.stmt config)) GCOr) ( let 
            [t1 (reduce (Config. (.test1 (.stmt config)) (.sig config))) 
            t2 (reduce (Config. (.test2 (.stmt config)) (.sig config)))] 
                (or t1 t2))
        
        (= (type (.stmt config)) GCTrue) true
        (= (type (.stmt config)) GCFalse) false



        (= (type (.stmt config)) GCSkip) (Config. (GCSkip.) (.sig config))
        (= (type (.stmt config)) GCAssign) ( let 
            [value (reduce (Config. (.expr (.stmt config)) (.sig config)))]
                (Config. (GCSkip.) (updateState (.sig config) (.symbol (.stmt config)) value)))
        (= (type (.stmt config)) GCComp) ( let 
            [first (reduce (Config. (.stat1 (.stmt config)) (.sig config)))]
                (reduce (Config. (.stat2 (.stmt config)) (.sig first))))
        (= (type (.stmt config)) GCIf) ( let [sample (rand-nth (.list (.stmt config)))]
            ( if (reduce (Config. (first sample) (.sig config)))   
                (Config. (last sample) (.sig config))
                (Config. (GCSkip.) (.sig config))))
            
        (= (type (.stmt config)) GCDo) ( let [sample (rand-nth (.list (.stmt config)))]
            
            (if (reduce (Config. (first sample) (.sig config)))  
                ( let [prev (Config. (last sample) (.sig config)) ]
                    (reduce (Config. (.stmt config) (.sig prev))))
                    (Config. (GCSkip.) (.sig config))))
    ))