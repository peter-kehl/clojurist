; Fibonacci 8: '(1 1 2 3 5 8 13 21)
( (fn [N]
    (case N
      [1] 1
      [2] [1 1]
      (loop [so-far [1 1] n 3 N N]
         (if (<= n N)
           (recur (conj so-far (+ (so-far (- n 3)) (so-far (- n 2)))) (inc n) N)
           so-far)))) 8)

;  http://www.4clojure.com/problem/27 palindrome?
( #(let [v (vec %)] (= v (reverse v))) "abcba")   
       
;http://www.4clojure.com/problem/28
( (fn [ [& items]] items) [1 2]) ;destructurizes both '(1 2) and [1 2], into a seq.
( (fn [in]
    (loop [[& in] in
           out '()]
      (if (empty? in)
        out
          (let [one (first in)
                others (rest in)] #_rest_can_return_empty
            (if (or (seq? one) (vector? one)) #_ (:instead :use coll?)
              (recur
                (if (empty? one)
                  (concat (rest one) others)
                  (concat [(first one)] (rest one) others))
                out)
              (recur
                others
                (concat out [one])))))))
 [1 2 [3 [4] '(5)]])
;from others:
(fn [ss]
  (letfn [(flt [s]
              (if (coll? s)
                (apply concat (map flt s))
                [s]))]
   (flt ss)))
(fn c [s] (if (sequential? s) (mapcat c s) [s])) ;non-tail recursive
(fn [s]
  (filter (complement sequential?)
    (tree-seq sequential? seq s)))

;http://www.4clojure.com/problem/solutions/29
((fn [st]
  (let [isUpperCh
        (fn [ch]  (re-find #"[A-Z]" (str ch)))]
   (apply str (filter isUpperCh st)))) 
 "cVaAbXyP")
;from others:
(fn [x] (clojure.string/join (re-seq #"[A-Z]" x)))
(fn [s] (apply str (filter #(Character/isUpperCase %) s)))
#(clojure.string/replace % #"[^A-Z]" "")
#(apply str (re-seq #"[A-Z]" %))

;http://www.4clojure.com/problem/30
(fn [in]
  (loop
    [in (sequence in)
     out [] #_ sequence_transforms_string_and_keeps_empty_seq]
    (if (empty? in) ; without this condition recursion run indefinitely, because (rest '()) returned '() again...
      out
      (if (empty? out)
        (recur (rest in) (vector (first in)))
        (if (= (first in) (last out))
          (recur (rest in) out)  #_skip
          (recur (rest in) (conj out (first in))))))))
 ;from others:
(fn [s
        (->> (partition 2 1 [0] s)
             (remove #(= (first %) (last %)))
             (map first))])
#(->> % (partition-by identity) (map first)) 
(fn [a]
  (map first (partition-by identity a)))
#(reduce (fn [a itm] (if (= (last a) itm) a (conj a itm))) [] %)

;http://www.4clojure.com/problem/31
((fn pack
  ([in] (pack in [])) ;can't recur into version with a different number of arugments!
  ([in out] (pack in out :nothing-repeated))
  ([in out prev]
   (if (empty? in)
     out
     (let [one (first in)]
      (if (= one prev)
        (recur 
          (rest in)
          (conj
            (pop out)
            (let [pushed (peek out)]
              (if (= pushed one)
               [one one] #_ (the first time it is repeated, hence make it into a list)
                
               (conj (peek out) one))))
          one)
        (recur
          (rest in)
          (conj out [one]) ;without making one into a vector it would be as per https://github.com/4clojure/4clojure/issues/305 
          one))))))
 [1 1 2 1 1 1 1 3 3])

;http://www.4clojure.com/problem/32
(reduce #(conj %1 %2 %2) [] [1 2 3])
;others:
(fn [x] (interleave x x))
(#(interleave % %) [1 2 3])
(fn [v] (reduce #(conj %1 %2 %2) [] v))
(#(mapcat vector % %) [1 2 3]) ;another use of #(...): to double/triple.... use of an expression, without repeating it, and without having an extra local bound symbol.
(#(mapcat list % %) [1 2 3])
#(mapcat (fn [a] (list a a)) %)

;http://www.4clojure.com/problem/33 no flatten, because that flattens any items that are collections themselves!
;Following is wrong. Read the spec!
((fn [seq rep]
   (apply concat (repeat rep seq)))
 [1 [2] 3] 4)

((fn [seq rep]
   (mapcat #(repeat rep %) seq))  
 [1 [2] 3] 4)
;others
#(apply concat (map (partial repeat %2) %1))
#(if (= %2 1) %1 (apply interleave (repeat %2 %1)))
fn [s n]
  (mapcat (partial repeat n) s)
#(mapcat (partial repeat %2) %1)

;http://www.4clojure.com/problem/34 implement range
( (fn [start end]
    (take (- end start)
      (iterate inc start)))
 -2 2)
;others
#(reductions + %1 (repeat (- %2 %1 1) 1)) ;<<<<
(reduce + 5 [1 2])
(reductions + 5 [1 2])
(fn [a b]
  (map-indexed + (repeat (- b a) a))); <<<<

(max-key #(/ 1 %) 1 2 3 0.3 0.2)
;http://www.4clojure.com/problem/38 implement max
( (fn [& seq] (* -1 (apply min (map #(* -1 %) seq))))
 1 8 3 4)    
( (fn [& seq]
    (reduce #(if (> %1 %2) %1 %2) seq))
  1 8 3 4)   
;others:
#(last (sort %&))
(fn [& nums] (->> nums (into (sorted-set)) last))
(fn [& args] (last (sort args)))
#(last (sort %&))
(fn [& s] (- (reduce min (map #(- %) s))))
;http://www.4clojure.com/problem/39 interleave
( (fn [a b]
    (mapcat #(vector %1 %2) a b))
 [1 2] [3 4 5 6])
(fn [a b]
    (mapcat vector a b))
;others
(mapcat list [1 2] [3 4 5 6])
    
;http://www.4clojure.com/problem/40 interpose
( (fn [a b]
    (butlast (mapcat #(vector % a) b)))
 0 [1 2 3])
;(__ 0 [1 2 3])
( (fn [b]
    (pop (mapcat vector b)))
 [1 2 3])
(assert (= (mapcat vector [1 2 3]) '(1 2 3)))
(pop '(1 2 3))
(type (mapcat vector [1 2 3])) ;LazySeq doesn't allow pop, but it allows rest and butlast!
(pop (mapcat vector [1 2 3]))   
(assert (= (butlast '(1 2 3)) '(1 2)))    
;others
(butlast (mapcat list %2 (repeat %1)))
(fn [d s] (rest (mapcat #(list d %) s)))
#(rest (mapcat (fn [a] [%1 a]) %2))
#(next (interleave (repeat %) %2))
(fn [sep [f & r]] (cons f (flatten (map #(list sep %) r))))

;http://www.4clojure.com/problem/41 drop every n-th item
( (fn [s n]
    (apply concat (map-indexed
                    (fn [index item] (if (not= (mod (inc index) n) 0)
                                       [item]
                                       []))
                    s)))
 [1 2 3 4 5 6 7 8] 3)
;others
#(keep-indexed(fn[a b](when(not=(dec %2)(mod a %2))b))%1) ;<<< keep-indexed
(fn drop-nth [s n]
  (when
    (seq s)
    (lazy-cat
      (take (dec n) s)
      (drop-nth 
        (drop n s)
        n))))
#(mapcat (partial take (dec %2)) (partition-all %2 %1)) ;<<partition-all
#(apply concat (partition-all (dec %2) %2 %))
#(map first
      (filter (comp (partial not= (dec %2))
                second)
        (map vector % (cycle (range %2))))) ;<< cycle

;http://www.4clojure.com/problem/42 factorial
#(reduce * (range 1 (inc %))) ; When typing (range) in InstaREPL, start with the boundaries!

;http://www.4clojure.com/problem/43 reverse interleave
(
 (fn [s n]
   (map-indexed
     (fn [i e])
     s))
   
   
 [1 2 3 4 5 6] 2)






  
          
          
      
      
    
    
    
























