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
   (reduce
     (fn [result [index entry]]
       (update-in ; 4Clojure has old Clojure without update
         result
         (list (mod index n)) ;cycle/partition between n sub-sequences
         #(conj % entry)))
       
     (vec (repeat n [])) ; When typing in InstaREPL, type boundaries first.
     (map-indexed
       (fn [i e]
         [i e])
       s)))
 
 #_( [1 2 3 4 5 6] 2)
 (range 10) 5)
;others
(fn [c n] (map #(take-nth n (drop % c))(range n))) ;<<< drop
(fn [s n]
    (map (fn [l] (map last l))
      (vals)
      (group-by ; <<<<<<<<<<<<<
        (fn [l] (mod (first l) n))
        (map-indexed list s))))
(group-by #(mod % 2) '(1 2 3)) ;values with unique key get wrapped in a vector anyway
#(vals (group-by (fn [itm] (mod itm %2)) %1))
#(apply map list (partition %2 %))
(fn [s x
         (map #(take-nth x (drop % s)) ;<<<<<< take-nth
              (range x))])

;http://www.4clojure.com/problem/44 rotate in *either* direction
; subseq, rsubseq work with sorted collections only!
( (fn [n s]
    (let [n (mod n (count s))]
      (concat (drop n s) (take n s))))
 2 [1 2 3 4 5])
(assert (= (mod -2 5) 3))
;others
#(let [idx (mod %1 (count %2))]
   (concat (drop idx %2) (take idx %2)))
#(take (count %2) (drop (mod % (count %2)) (cycle %2)))
#(take (count %2) (drop (mod %1 (count %2)) (flatten (repeat 2 %2)))) ;cycle = flatten (repeat ...)
(fn [n s]
  (let [[a b] (split-at (mod n (count s)) s)]
    (concat b a)))
; also take-last, drop-last
; (->> a (take 7) (drop 3)) ; get 4 elements starting from 3

;http://www.4clojure.com/problem/46 higher order reverse args
((
  #(
     fn [ & args]
     (apply % (reverse args)))
  nth)
 2 [1 2 3 4 5])

;http://www.4clojure.com/problem/49 split-at
((fn [n s]
   [(take n s) (drop n s)])
 3 [1 2 3 4 5 6])
        
((fn [n s]
   [(take n s) (drop n s)])
 3 [1 2 3 4 5 6])
 ;others         
(juxt take drop)

;http://www.4clojure.com/problem/50 split by type 
( (fn [c] (vals
            (group-by
             #(type %)
             c)))
 [1 :a 2 :b 3 :c])
;others
#(vals (group-by type %)) ;vals = map second
#(map second (group-by type %)) ;(map identity {:i 1}) shows that function "map" applied to a map passes entries as a vector [key value].

;http://www.4clojure.com/problem/53 Longest Increasing Sub-Seq
((fn [s]
   (let [winner
         (fn [candidate result]
             (if
               (and
                (> (count candidate) (count result)) ;Don't shorten into > (count candidate) (count result) 1, because initially result is empty.
                (> (count candidate) 1))
               candidate
               result))]
     (loop [s s result [] candidate []]
          (if (empty? s)
            (winner candidate result)
            (if (empty? candidate)
              (recur (drop 1 s) result [(first s)])
              (let [sFirst (first s)]
                (if (> sFirst (last candidate))
                  (recur (drop 1 s) result (concat candidate (list sFirst)))
                  #_ "else_capture_candidate_if_winner_and_restart_the_search. However, that doesn't capture if the winner is the tail."
                  (recur (drop 1 s) (winner candidate result) [sFirst]))))))))
 
 
 [2 3 3 4 5] #_[7 6 5 4] #_[1 0 1 2 3 0 4 5])
; Sequence literals don't recognise bound symbols! Don't use '(variableName) but (list variableName). 
;others
(fn [s] 
    (->>
      (for [a (range (count s)) 
            b (range (inc a) (count s))]
        (subvec s a (inc b)))
      (filter #(apply < %))
      (sort-by count >)
      first
      vec))
;
complement ;higher order
;https://clojuredocs.org/clojure.core/for
(time (dorun (for [x (range 10) y (range 100) :when (> x y)] [x y])))

;http://www.4clojure.com/problem/54 partition
( (fn [n in]
    (map
      (fn [vec-of-pairs]
        (map
          #(:item %)
          vec-of-pairs))
      (filter
        #(= (count %) n)
        (vals
          (group-by #(int (/ (:index %) n)) ;could shorten with: quot
            (map-indexed
              (fn [index item]
                {:index index :item item})
              in))))))
 3 (range 2 10))
;others
(fn [x s]
  (map #(take x (drop % s)) (range 0 (- (count s) x -1) x)))
(fn [n s]
  (loop [in s out []]
    (if (> n (count in))
      out
      (recur (drop n in) (conj out (take n in))))))
#(filter (fn [m] (= (count m) %1)) (map last (group-by (fn [n] (quot n %1)) %2)))
(fn part [n l]
  (loop [remaining l
         parts (transient [])]
    (if (>= (count remaining) n)
      (let [[h t] (split-at n remaining)]
        (recur t (conj! parts h))) ; transient conj! persistent!
      (persistent! parts))))

;;http://www.4clojure.com/problem/55 occurrences/ frequencies
((fn [in]
   (reduce
     (fn [out item]
       (let [existing
             (some
               (fn [entry]
                 (if (= (key entry) item)
                   entry))
               out)] ;some on a map returns [key value] but a special one: you can apply (key) and (val)
         (assoc out
           item
           (if existing
             (inc (val existing))
             1))))
     {}
     in))
 ;[1 1 2 3 2 1 1]
 [:b :a :b :a :b])
 ;'([1 2] [1 3] [1 3]))
;entries as results of group-by - it works even if entries can be collections.
((fn [in]
  (reduce
    (fn [out [k v]]
      (assoc out k (count v)))
    {}  
    (group-by identity in)))
 '([1 2] [1 3] [1 3]))
;others
#(apply merge-with + (map (fn [x] {x 1}) %))
#(into {} (for [[k v] (group-by identity %)] [k (count v)]))
#(into {} (map (fn [[_ [v :as vs]]] [v (count vs)]) (group-by identity %)))
partial reduce 
         #(assoc %1 %2 (+ 1 (or (get %1 %2) 0)))
         {}
#(reduce (fn [res [k v]] (assoc res k (count v)))
         {}
         (group-by identity %))
;replace remove frequencies

;http://www.4clojure.com/problem/56 distinct
;not-any?
; Following failed 4th test only - because group-by doesn't keep the order!
((fn [in]
   (keys
     (group-by
       identity
       in)))
 (range 50))
 ;[1 2 1 3 1 2 4])

 ; Don't use simple set/map, because they don't keep the insertion order
(into (sorted-map) [[:l 4] [:k 3] [:j 2] [:i 1]]) ;(into {} [[key value]...]) but not (into {} existing-map)!
(into (sorted-set) [4 1 5 3])
((fn [in]
   (map
     (fn [{index :index value :val}]
       value)
     (into
       (sorted-set-by ;re-order by origial index
         (fn [{index-a :index} {index-b :index}]
           (< index-a index-b)))
       (into
         ; into honours the collection/set/map type and the *comparator*, too.
         (sorted-set-by ; it doesn't accept an existing map, hence a need for a higher (into)
           (fn [{a :val} {b :val}]
             (compare a b))) ;Don't use <, because it fails for non-numbers (e.g. keywords and strings)!
         (map-indexed
           (fn [index value]
             {:index index :val value})
           in)))))
 '([2 4] [1 2] [1 3] [1 3]))
 ;(range 50))

; Proof that (into) carries a comparator
(assert
  (=
   (count
     (into
       (sorted-set-by
         #(< (mod % 4) (mod %2 4))
         0 1)
       [0 4 1 2])
     3)))
; keep-indexed

((fn [in]
   (reduce ; << like filter with recursion to see existing results
     (fn [out item]
       (if (some
             #(= % item)
             out)
        out
        (conj out item)))
       
     []
     in))  
     
 (range 50))
(assert (not (contains? [4 5] 4))) ;contains? searches by index. Instead, use some
;others
#(reduce (fn [v n]
             (if (some #{n} v)
               v
               (conj v n))
           [] %))
#(reduce (fn [a itm] (if (some (fn [elm] (= itm elm)) a) a (conj a itm))) [] %)
reduce #(if ((set %) %2) % (conj % %2)) []

;http://www.4clojure.com/problem/57 recursion
(fn foo [x] (when (> x 0)
              (conj (foo (dec x)) x)))
(assert (= (conj nil 1) '(1))) ;<<< nil becomes '() but only if passed as a collection arg

;http://www.4clojure.com/problem/58 comp
( ( (fn [& fns]
      (if (empty? fns)
        identity
        (loop [result (last fns) fns (take (dec (count fns)) fns)]
          (if (empty? fns)
            result
            (recur 
              #((last fns)
                (apply
                  result
                  %&))
              (take (dec (count fns)) fns))))))
             
   rest reverse)
 [1 2 3 4])
; (rest) is better than (next), because it it returns an empty sequence rather than nil.
; Then use (if (empty?)) rather than direct (if). Benefit: If you change to a different source
; e.g. (take), the (if) condition stays. 
; others
(fn [& args] (reduce (fn [a b] (fn [& more] (a (apply b more)))) args))
(fn [& fs]
  (fn [& xs]
    (first (reduce #(vector (apply %2 %1)) xs (reverse fs)))))


; http://www.4clojure.com/problem/59 juxt
((
  (fn [ & fns]
    (fn [& args]
      (map
        #(apply % args)
        fns)))
  + max min)
 2 3 5 1 6 4)






































































