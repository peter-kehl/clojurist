(require 'clojure.set)
(require 'clojure.pprint)

;---------------------------------------------

;http://www.4clojure.com/problem/82 word chains
(if false ; attempt via tail recursion TODO
  ((fn [wset]
     (let [ws (vec wset)]
       (loop [parent-path []]
             (if (= (count parent-path) (count ws))
               true
               (let []))))) 
         
   
   #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"}))

(if true
  ((fn [wset]
     (let [ws (vec wset)]
       (letfn
         [(chain [parent-path node-candidates]
               (if (= (count parent-path) (count ws))
                 true
                 (for [node node-candidates
                       :when true] true)))]
         (chain [] ws)))) 
   
   
   #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"}))

(if false
  ((fn [wset] ;1. Create map {word derivative(s)}. 2. Find a chain.
     (let [ws (vec wset)
           ders (reduce ;ders will be a map of derivatives: {from1 [to1-1 to1-2 to1-3...] from2 [to2-1 to2-2...}
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
  
;http://www.4clojure.com/problem/83 half truth
#(boolean (and (some true? %&) (some false? %&))) ; could simplify: true? => identity, false? => not
;others
#(= 2 (count (set %&))) ; >>> SIMPLIFY
(fn [& bools] (apply not= bools))
(comp (every-pred #(not-every? false? %) #(not-every? true? %)) list) ;<< TODO
; every? not-every?
(fn [& args] 
  (and 
   (reduce #(or %1 %2) args)
   (not (reduce #(and %1 %2) args)))) ; or: instead of reduce, use apply.

;http://www.4clojure.com/problem/84 Transitive Closure TO

;http://www.4clojure.com/problem/85 Power Set
;Generating combinations based on binary representations of 0 inclusive... 2^number-of-items exclusive.
;each binary digit indicates whether the respective item is present in the set
(if false
  ((fn [all-set]
     (let [all (vec all-set)
           pow2 (memoize #(apply * (repeat % 2))) ;good that (*) with no params returns 1
           magnitude (count all)
           numbers (range 0 (pow2 magnitude))]
       (set
         (for [n numbers]
             (set
               (for [i (range 0 magnitude)
                       ;Can't use :when (not (zero? (mod n (pow2 (inc i))))))] - because (mod 2 4) is non-zero!
                       :when (not (zero? (mod
                                           (- n (mod n (pow2 i)))
                                           (pow2 (inc i)))))]
                 ;<<< CONSIDER reduce with a compound result
                    (all i)))))))
     
   #{:a :b}))
; 123 = 1x100 + 2x10 + 3
;others
(fn [t]
    (set
      ((fn f [[x & s]]
         (if x
           (let [z (f s)] (concat z (map #(conj % x) z)))
           #{#{}})
        (seq t)))))
(fn [s]
    (set (reduce #(concat %1 (map (fn [i] (set (conj i %2))) %1)) (list #{}) s)))

(fn [s]
  (reduce
    (fn [ps x]
     (reduce (fn [ps s])
       (conj ps (conj s x))) ps ps)) #{#{}} s)
(fn [s]
  (letfn
    [(rsets [root [head & tail]] ;
      (if
        (nil? head)
        [root]
        (concat
          (rsets root tail)
          (rsets (conj root head) tail))))]
    (set (map set (rsets [] (apply list s))))))
(fn [original]
  (loop [result #{}, to-process [original]]
    (if (empty? to-process)
      result
      (let [s (peek to-process)
            new-sets (filter (complement result) (map #(disj s %) s))] ;<< disj
        (recur (conj result s) (into (pop to-process) new-sets))))))
(fn power-set
  ([s] (power-set (count s) s))
  ([k s]
   (if (= k 0)
     #{#{}}
     (let [subsets (power-set (dec k) s)]
       (into subsets
             (for [x s
                   ss subsets]
               (conj ss x)))))))
reduce
  (fn [a x] (into a (map #(conj % x) a)))
  #{#{}}
;(#(apply * (repeat % 2)) 3)

;http://www.4clojure.com/problem/86 happy numbers
(if false
  (fn [start]
    (loop [n start
           sums #{}]
      (let [sum
            (loop [digit (mod n 10)
                   prefix (int (/ n 10))
                   result 0]
              (if (zero? prefix)
                (+ result (* digit digit))
                (recur
                  (mod prefix 10)
                  (int (/ prefix 10))
                  (+ result (* digit digit)))))]
        (cond
          (sums sum) false
          (= sum 1) true
          :else (recur sum (conj sums sum)))))))
;others
(fn [t]
    (= 1
       ((fn f 
          ([x d]
           (let [s (reduce + (map (comp #(* % %) read-string str) x))]
             (if (d s)
               s
               (f (str s) (conj d s)))
             ([x] (f (str x) #{})))))
        t)))
(fn [x]
  (loop [x x seen '()]
    (cond (= 1 x) true
          (some #(= % x) seen) false
          true
            (recur 
              (reduce +
                (map #(int (Math/pow ;<<<<
                            (Integer/parseInt (str %)) 2))
                     (str x)))
              (conj seen x)))))
(fn [n]
  (let [sqs (fn [x] (apply + (map (comp #(* % %) read-string str) (str x))))]
    (->> [[] n]
      (iterate (fn [[a x]]
                 (cond (= x 1) true
                   (some (partial = x) a) false
                   :else [(conj a x) (sqs x)])))
      (drop-while vector?)                            ;<<<
      (first))))
                  
;http://www.4clojure.com/problem/88 symmetric difference
(if true
  (fn [one two]
    (set (filter
           #(not (and (one %) (two %)))
           (into one two)))))
;others
#(set `(~@(% %2 %3) ~@(% %3 %2))) remove ;<<<<< ????? TODOOO
(fn [a b] 
  (clojure.set/difference 
    (clojure.set/union a b) 
    (clojure.set/intersection a b)))
(fn [a b]
    (clojure.set/union (clojure.set/difference a b) (clojure.set/difference b a)))
#(set (concat (remove % %2) (remove %2 %)))

          






















































