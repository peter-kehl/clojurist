(if false
; http://www.4clojure.com/problem/61 zipmap
  ((fn [ks vs]
     (reduce
       (fn [m [k v]]
         (assoc m k v))
       {}
       (map
         (fn [k v]
           [k v])
         ks
         vs)))
   
   [:a :b :c] [1 2 3]))
  
(into {} '((:i 1))) ;fails
(into {} '([:i 1])) ;OK
(into {} [[:i 1]]) ;OK
(assoc {} :i 1) ;OK
;others
#(apply hash-map (mapcat list %1 %2))
#(apply hash-map (interleave %1 %2))
(fn [keys values]
  (apply array-map (interleave keys values)))
(fn [ks vs] (apply merge (map (fn [k v] {k v}) ks vs)))
(fn [a b]
   (apply assoc {} (interleave a b)))
#(into {} (map vector %1 %2))

(if false
  ;http://www.4clojure.com/problem/62 iterate
 (take 5 (
          (fn it [f x]
            (cons x
              (lazy-seq (it f (f x)))))
          #(* 2 %) 1)))
;others
(fn g[f x] (lazy-seq (cons x (g f (f x)))))

;http://www.4clojure.com/problem/63 group-by
(if false
  ((fn [f s]
     (reduce
       (fn [so-far k]
         (let [value (f k)
               group (into (so-far value []) [k]) ;accessing an existing map entry, otherwise proposing an empty vector as default
               groups (into so-far [[value group]])]
           groups))
       {}
       s))
   #(> % 5) [1 3 6 8]))
(into nil [1 2]) ; works with nil as first arg, but re-orders items from the second arg, and converts into a sequence, even though it's a vector!
(into [1 2] [3 4]) ; honours order of both vectors, and returns a vector
;others
#(apply merge-with concat (map (fn [x] {(%1 x) [x]}) %2))
(fn [f s]
  (reduce (fn [m a])
    (let [x (f a)]
      (assoc m x (conj (get m x []) a)))) {} s)
(fn [f s] (reduce (fn [a itm] (assoc a (f itm) (concat (a (f itm)) [itm]))) {} s))
(fn [f coll]
  (reduce 
   (fn [m x] (assoc m (f x) (conj (or (m (f x) [])) x)))
   {} coll))

;http://www.4clojure.com/problem/65 "black box testing" i.e. collection/map type
;merge into itself. If same (count) => map/set.
;  Then (into) [[:special_key :special_value]], retrieve back by (coll :special_key) - if nil then it's a set (rather than a map).
; 4Clojure should change 'list' to be 'sequence'
(if false
  (map
           (fn [compound]
             (let [throws?
                   (fn [f & args]
                     (try
                       (apply f args)
                       false
                       (catch Exception e true)))]
               (cond
                 (throws? #(into compound [:no-pair])) :map
                 (let [added (into compound [1 #_anything-to-ensure-non-empty])]
                   (= added (into added [1]))) :set
                 (= (last (conj compound :special-item)) :special-item) :vector
                 :else :list)))
           
           [{} #{} [] () {:a 1, :b 2} #{10 (rand-int 5)} [1 2 3 4 5 6] (range (rand-int 20))]))
(use '[clojure.test]) ;namespace
assert-expr

(vec {5 6})
(vec #{5 6})
(= [5 6] #{5 6}) ; false => from (vec coll) we can detect if it's a sequence/vector, or a map/set 
(def typeOf
  (fn [given]
     (let [veccy (vec given)
           seqOrVec (= veccy given)]
      
       (if seqOrVec
         (if
           (= (last (conj given :special1 :special2)) :special2) ;must add two items, to differentiate an empty vector from an empty sequence
           :vector
           :list)
         (let [added (into given [[:special-key :special-value-1]])]
           (if
             (=
                (count added)
                (count
                  (into added [[:special-key :special-value-2]])))
             :map
             :set))))))
(if false
  (map
   typeOf
   
   [{} #{} [] () {:a 1, :b 2} #{10 (rand-int 5)} [1 2 3 4 5 6] (range (rand-int 20))]))
(if false
  (typeOf {:a 1, :b 2}))  
;others
fn [x]
  (let [z (into x [[x 1] [x x] [x 1] [x x]])]
    ({1 :map 2 :set 4 (if (= [x x] (first z)) :list :vector)
      (- (count z) (count x))}))
(comp {\# :set \{ :map \[ :vector \c :list} first str); cheating. But: good use of a {map...} as function
#(condp = (first (str %))
   \[ :vector
   \{ :map
   \# :set
   :list)
(fn [item]
  (cond
    (= (conj item [:test 0] [:test 1])
       (conj item [:test 1])
      :map)
    (= (conj item [:test 0])
       (conj item [:test 0] [:test 0])
      :set)
    (= (last (conj item :test :test2))
       :test2
      :vector)
    :else
      :list))
(fn [coll]
    (let [base (empty coll)] ;Good: empty
      (cond
        (= base {}) :map
        (= base #{}) :set
        (reversible? base) :vector
        (= base '()) :list)))
(fn black-box-testing [x]
  (let [e (empty x)]
    (cond (= e {}) :map
          (= e #{}) :set
          (and (= e '()) (= :TEST (first (conj e nil :TEST)) )) :list
          (and (= e []) (= :TEST (last (conj e :nil :TEST))  )) :vector

          :else :unknown)))
#(case ((juxt associative? reversible? ifn?) %)
   [true  true  true ] :vector
   [false false false] :list
   [true  false true ] :map
   [false false true ] :set)

;http://www.4clojure.com/problem/66 the greatest common divider
(if true
 ((fn [a b]
    (loop [a a b b cand (min a b)]
      (if (= (mod a cand) (mod b cand) 0)
        cand
        (recur a b (dec cand)))))
  1023 858))
;ohers
(fn g [a b] (cond (> a b) (g b a) (= 0 a) b :t (g a (rem b a)))) ;also through recur
(fn gcd [a b] (if (zero? b) a (gcd b (mod a b))))

;http://www.4clojure.com/problem/67 n first prime numbers
(if false
  (last ((fn [n]
          (loop [primes [2]]
            (if (= (count primes) n)
              primes
              (let [prime
                    (loop [primeCandidate (inc (last primes))]
                      (let [primeCandidateSqrt (Math/sqrt primeCandidate)
                            divider
                            (loop [dividerCandidateIndex 0]
                              (let [dividerCandidate (primes dividerCandidateIndex)]
                                (if (<= dividerCandidate primeCandidateSqrt)
                                  (if (not= (mod primeCandidate dividerCandidate) 0) ;use zero?
                                    (recur (inc dividerCandidateIndex))
                                    dividerCandidate)
                                  nil)))]
                        (if (not divider)
                          primeCandidate
                          (recur (inc primeCandidate)))))]
                (recur (conj primes prime))))))     
         100)))
;others
(fn [n]
  (take n
    (filter 
      (fn [x] 
        (not-any? #(= 0 (mod x %)) (range 2 x))) (iterate inc 2))))
(fn [cnt]
  (let [not-divisible? #(not (zero? (mod %1 %2)))
        is-prime? #(every? (partial not-divisible? %)
                           (range 2 (inc (Math/sqrt %))))]
   (take cnt (concat [2] (filter is-prime? (iterate inc 2))))))
(fn [cnt]
  (let [not-divisible? #(not (zero? (mod %1 %2)))
        is-prime? #(every? (partial not-divisible? %)
                           (range 2 (inc (Math/sqrt %))))]
   (take cnt (concat [2] (filter is-prime? (iterate inc 2))))))

;http://www.4clojure.com/problem/69 merge-with
(if true
  ((fn [f & maps]
     (reduce
       (fn [result mp]
         (reduce
           (fn [res [k v]]
             (assoc res k
               (if (contains? res k)
                 (f (res k) v)
                 v)))
           result
           mp))
       {}
       maps))  
   * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5}))
;others
(fn [f & m]
  (reduce
    (fn [m [k v]] (conj m [k (if (m k) (f (m k) v) v)]))
    {}
    (apply concat (map seq m))))
(fn [f & maps]
  (->> maps
    (mapcat (partial into []))
    (group-by key)
    (map (fn [[k kvps]] [k (reduce f (map second kvps))]))
    (into {})))

;http://www.4clojure.com/problem/70 sort words only, case insensitive
(if false
 ((fn [sentence]
    (sort
      #(compare (.toUpperCase %) (.toUpperCase %2))
       (clojure.string/split sentence #"[ \t,.!]+")))
  "Have a nice day."))
;others
#(sort-by (memfn toUpperCase) (re-seq #"\w+" %))
(fn [s]
  (sort-by clojure.string/lower-case)
  (re-seq #"\w+" s))
#(->> (clojure.string/split % #"\W+")
      (sort-by clojure.string/lower-case))
#(sort-by clojure.string/lower-case (re-seq #"\w+" %))

;http://www.4clojure.com/problem/71 rearrange code ->
; "form" means a [partial] expression
;http://www.4clojure.com/problem/72 (apply + seq) may be more efficient than (reduce + seq)

;http://www.4clojure.com/problem/73 Tic Tac Toe
(def tic
  (fn [[row0 row1 row2]]
      (let [won
            (fn [[a b c]]
              (or
                  (and
                      (= a b c)
                      (some #{a} [:x :o])
                      a) ;'a' makes (and) return value of a on succes (instead of boolean)
                  nil))] ; 
        
        (or
            (won row0) (won row1) (won row2)
            (let [column
                  (fn [col]
                    [(row0 col) (row1 col) (row2 col)])]
              (or
                  (won (column 0)) (won (column 1)) (won (column 2))
                  (won [(row0 0) (row1 1) (row2 2)])
                  (won [(row0 2) (row1 1) (row2 0)])))))))
(tic [[:e :e :e]
      [:e :e :e]
      [:e :e :e]])
(tic [[:x :e :o]
      [:x :e :e]
      [:x :e :o]])
(tic [[:x :e :o]
      [:x :x :e]
      [:o :x :o]])
;others
(fn [d]
  (let [n [0 1 2]
        s (set
            (concat
              d
              (apply map list d)
              [(for [a n] (get-in d [a a]))
               (for [a n] (get-in d [(- 2 a) a]))]))]
    (cond
      (s [:x :x :x]) :x
      (s [:o :o :o]) :o
      :e nil)))
;and many others

;http://www.4clojure.com/problem/74 filter perfect squares
((fn [str]
   (String/join "," ;onl since Java 8 - not at 4clojure.com
     (let [in (re-seq #"[0-9]+" str)]
          (filter
            #(let [num (Integer/parseInt %)
                   root (int (Math/sqrt num))]
               (= num (* root root)))
            in))))
 "4,5,6,7,8,9")
((fn [st]
   (reduce
     (fn [out num]
       (str out "," num))
     (let [in (re-seq #"[0-9]+" st)]
          (filter
            #(let [num (Integer/parseInt %)
                   root (int (Math/sqrt num))]
               (= num (* root root)))
            in))))
 "4,5,6,7,8,9")
;others
(fn [s]
    (->> (read-string (str \[ s \]))
      (filter #(let [q (Math/sqrt %)] (= % (* q q))))
      (interpose \,)
      (apply str)))
(fn [s]
  (apply str (interpose ",")
    (filter #(let [root (Math/sqrt %)] (= root (int root)))
      (map #(Integer/parseInt %) (.split s ",")))))
(fn [s]
   (->> s
        (#(clojure.string/split % #","))
        (map read-string)
        (filter #(== (Math/sqrt %) (int (Math/sqrt %))))
        (clojure.string/join ",")))
(fn [s]
  (->> (clojure.string/split s #",")
       (filter #(zero? (mod (Math/sqrt (Double. %)) 1)))
       (clojure.string/join ",")))

;http://www.4clojure.com/problem/75 Euleur's Totient function: number of coprimes lower than x coprime to x.



































