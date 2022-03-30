(defn replace-value-in-termstring

    [value replacement string]
    (clojure.string/replace
     string

     (re-pattern (str "(\\A|\\s|\\[|\\{|\\()(" value ")(\\)|\\}|\\]|\\s|\\z)"))
     (str "$1" replacement "$3")))
  
  (defn replace-values-in-termstring

    [values replacements string]
    (if (and (seq values) (seq replacements))
      (let [[v & vs] values
            [r & rs] replacements]
        (replace-values-in-termstring vs rs
                                      (replace-value-in-termstring v r string)))
      string))
  
  (defn closing-paren-in-string
    [s]
    (letfn [(nth-closing-paren-in-string [s openings]
              (cond

                (> 2 (count s)) `(~(apply str s) "")
                :else
                ;; Otherwise, decompose `s` to check its first character.
                (let [[c & cs] s]
                  (cond

                    (= c \() (let [[before after] (nth-closing-paren-in-string cs (+ openings 1))]
                                `(~(str c before) ~(apply str after)))

                    (= c \)) (if (= openings 0)
                               `(~(str c) ~(apply str cs))
                               (let [[before after] (nth-closing-paren-in-string cs (- openings 1))]
                                  `(~(str c before) ~after)))
                    ;; Otherwise, just make the recursive call and prepend `c` to the returned first string.
                    :else     (let [[before after] (nth-closing-paren-in-string cs openings)]
                                `(~(str c before) ~after))))))]
      ;; Start out with 0 opening parentheses seen.
      (nth-closing-paren-in-string s 0)))
  
  (defn replace-call-in-termstring

    [f replacement string]
     ;; Find the first occurrence of `f` preceded by an opening parenthese.
     (let [m (re-find (re-pattern (str "(.*)\\((" f "\\s.*)")) string)]
       (if m
         ;; Get the parts of m.
         (let [[whole before call-and-after] m
               ;; Separate `call-and-after` at the first unmatched closing parenthese.
               [callbody after] (closing-paren-in-string call-and-after)]
           (str before replacement after))
         ;; If m is null, just return the string as is.
         string)))
  
  (defmacro unwindrec
    ([name args basecond basebody reccond recbody]
     ;; If `elsebody` is not provided, substitute `nil` instead.
     `(unwindrec ~name ~args ~basecond ~basebody ~reccond ~recbody nil))
    ([name args basecond basebody reccond recbody elsebody]
     ;; Define the function `name` taking arguments `args`.
     `(defn ~name ~args

        (letfn
            [(~'f [~'context ~@args]
              ;; Use the provided base case and recursive case conditions.
              (cond

                ~basecond (let [~'value ~basebody]
                            (println (replace-value-in-termstring
                                      '~'rec
                                      ~'value
                                      ~'context))
                            ~'value)

                ~reccond  (let [~'this-context-with-call (replace-values-in-termstring
                                                          '~args
                                                          (map str ~args)
                                                          (replace-value-in-termstring
                                                           '~'rec
                                                           (str '~recbody)
                                                           ~'context))

                                ~'this-context-with-rec (replace-call-in-termstring
                                                         '~name
                                                         "rec"
                                                         ~'this-context-with-call)]
                            (println ~'this-context-with-call)
       
                            (let [~'result ~(clojure.walk/prewalk-replace {name '(partial f this-context-with-rec)} recbody)
                                  ~'this-context-with-result (replace-value-in-termstring '~'rec (str ~'result) ~'context)]
                              ;; Print out the context again with the result in place, unless

                              (when (not= ~'this-context-with-result (str ~'result))
                                (println ~'this-context-with-result))
                              ~'result))
                :else ~elsebody))]
  
          ;; Before calling `f`, print out the originating call.
          (println (replace-values-in-termstring
                    '~args
                    (map str ~args)
                    (str "(" (clojure.string/join " " '(~name ~@args)) ")")))
          ;; Then call `f`.
          (~'f "rec" ~@args)))))

          
;;Actual Work start here


(unwindrec exponent [n e]
        (= e 0) 1
        (> e 0) (* n (exponent n (- e 1)))
        (throw (Exception. "Trying to calculate exponent of a negative number.")))


(defn exponent-tr [n e]
    (unwindrec exponent-tr-helper [n e collect]
        (= e 0) collect
        (> e 0) (exponent-tr-helper n (- e 1) (* collect n))
        (throw (Exception. "Trying to calculate exponent of a negative number."))

    )
    (exponent-tr-helper n e 1))


(unwindrec sumlist [xs]
    (= (empty? xs) true) 0
    (= (empty? xs) false) (+ (first xs) (sumlist (rest xs)))
    
)

(defn sumlist-tr [xs]
    (unwindrec sumlist-tr-helper [collect xs]
        (= (empty? xs) true) collect
        (= (empty? xs) false)    (sumlist-tr-helper (+ collect (first xs)) (rest xs))
    )
    (sumlist-tr-helper 0 xs))

(unwindrec flattenlist [xs]
    (= (empty? xs) true) ()
    (= (empty? xs) false) (concat (first xs) (flattenlist (rest xs)))
    
)

(defn flattenlist-tr [xs]
    (unwindrec flattenlist-tr-helper [collect xs]
        (= (empty? xs) true) collect
        (= (empty? xs) false) (flattenlist-tr-helper (concat collect (first xs)) (rest xs))
    )
    (flattenlist-tr-helper '() xs))


(unwindrec postfixes [xs]
    (= (empty? xs) true) (conj '() '())
    (= (empty? xs) false) (conj (postfixes (rest xs)) xs)
    
)

(defn postfixes-tr [xs]
    (unwindrec postfixes-tr-helper [collect xs]
        (= (empty? xs) true)  (reverse (conj collect '()))
        (= (empty? xs) false) (postfixes-tr-helper (conj collect xs) (rest xs))        
    )
  (postfixes-tr-helper '() xs))