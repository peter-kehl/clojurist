;---------------------------------------------

;http://www.4clojure.com/problem/82 word chains
(if true
  ((fn [wset] ;1. Create map {word derivative(s)}. 2. Find a chain.
     (let [ws (vec wset)
           ders (reduce ;ders will be a map of derivatives: {from [to1 to2 to3...]}
                  (fn [mp [from to]]
                    (merge mp
                      (if (mp from)
                        {from (conj (mp from) to)}
                        {from to})))
                  {}
                  (concat
                    (for [from wset
                          to wset :when (not= from to)]
                      (if true ;(not-empty [])
                        [[from to]]
                        []))))]
          (loop [words-todo-in-stack-above ws
                 words-todo-this-level ws
                 from nil
                 is-1st-level true]
            (assert (= (nil? from) is-1st-level))
            (if is-1st-level
              false; (recur words-todo-in-stack-above (rest words-todo-this-level) (first words-todo-this-level) false)
              
              (let [words-todo (remove #{from} words-todo-this-level)
                    unknown      (cond
                                   (ders from) true
                                   (empty? words-todo-this-level #_maybe) false)]))
            true
            (empty? words-todo-in-stack-above ) false)))
            ;:else (recur (rest words-todo-in-stack-above
  
   #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"}))
  





































