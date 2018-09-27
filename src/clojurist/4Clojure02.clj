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

(defn throws? [f & args]
    (try
      (apply f args)
      false
      (catch Exception e true)))
(assert (throws? #(into {} '((:i 1)))))
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
  (reduce
    (fn [m a]
       (let [x (f a)]
         (assoc m x (conj (get m x []) a))))
    {} s))
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
(fn [x]
  (let [z (into x [[x 1] [x x] [x 1] [x x]])]
    ({1 :map
      2 :set
      4 (if (= [x x] (first z))
          :list :vector)}
     (- (count z) (count x)))))
(comp {\# :set \{ :map \[ :vector \c :list} first str); cheating. But: good use of a {map...} as function

#(condp = (first (str %)) ; cond with a predicate <<<<
   \[ :vector
   \{ :map
   \# :set
   :list)
(fn [item]
  (cond
    (= (conj item [:test 0] [:test 1])
       (conj item [:test 1]))
    :map
    (= (conj item [:test 0])
       (conj item [:test 0] [:test 0]))
    :set
    (= (last (conj item :test :test2))
       :test2)
    :vector
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
(if
  false
  ((fn [num]
       (count
         (if
           (= num 1)
           '(1)
           (let [prims
                 ((fn [m] ;from http://www.4clojure.com/problem/67 n first prime numbers, but adjusted
                    (loop [primes [2]]
                      (if (>= (last primes) m)
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
                  num)]
             (filter
               (fn [coprime]
                 (not ;no common divisors
                   (some
                     (fn [divisor]
                       (= (mod coprime divisor) (mod num divisor) 0))
                     (filter
                       (fn [divisorCandidate]
                         (<= divisorCandidate coprime))
                       prims))))
               
               (range 1 num))))))
   16))
 ; others
(fn [s]
  (letfn [(g [a b] (if (zero? b) a (g b (mod a b))))]
    (count
      (filter
        #(== 1 (g s %))
        (range s)))))
(use 'clojure.set)
#(let [divisors (fn [n] (set (filter (fn [dem] (= 0 (mod n dem))) (range 2 (inc n)))))]
  (if (= % 1) 1
    (count (filter (fn [n] (empty? (clojure.set/intersection (divisors n) (divisors %)))) (range 1 %))))) 
(fn [n]
  (if (= n 1) 1
    (let [gcd (fn f [a b] (if (zero? b) a (f b (mod a b))))]
      (count (filter #(= 1 (gcd n %)) (range 1 n))))))

;http://www.4clojure.com/problem/76 trampoline: for mutual recursion
(if
  false
  (letfn
    [(foo [veccy y] #(bar (conj veccy y) y))
     (bar [veccy y] (if (> (last veccy) 10)
                      veccy
                      #(foo veccy (+ 2 y))))]
    (trampoline foo [] 1)))
; use letfn to declare mtually recursive functions (a function can call another one defined *later*)

;use not-empty, seqable?
;bound?

;http://www.4clojure.com/problem/77 anagram finder
(if
  false
  ((fn [words]
      (set (filter
             #(> (count %) 1); => shorten to: remove #(= 1 (count %))
             (map set
               (vals
                 (group-by
                   #(sort (seq %)) ; => shorten to: sort
                   words))))))
   
   ["veer" "lake" "item" "kale" "mite" "ever"]))
; remove
; others
(fn [words] 
  (set (map set) 
    (filter 
      #(< 1 (count %)) 
      (vals 
        (group-by frequencies words)))))
; frequencies
#(set (for [v (vals (group-by sort %))
            :when (> (count v) 1)]
        (set v)))
; for :when

;http://www.4clojure.com/problem/78 trampoline
(if true
  (fn [f & args]
    (loop [f f args args]
      (if
        (ifn? f) ;detect whether it's a function. Don't use (fn? ...)
        (recur (apply f args) [])
        f))))
;others
(fn [f & args]
  (loop [f (apply f args)]
    (if (fn? f)
      (recur (f))
      f)))
;-------------

;http://www.4clojure.com/problem/79 triangle minimal path
;TODO memoized local recursive function. It would save 1/4 time.
;https://stackoverflow.com/a/13123571/1049315 and/or with-local-vars
;http://danmidwood.com/content/2013/02/24/exploring-clojure-memoization.html
(defmacro trace [body]
  (list 'try body
    (list 'catch 'Exception 'e (list 'println "ho"))))
(macroexpand `(trace ((first subtree) col)))

(if false
  ((fn [tree]
     (letfn [(sub-paths [subtree col parent-path] ;subtree is a (take N tree); col is a 0-based index in the first row
               (let [entry ((first subtree) col)
                     my-path (conj parent-path entry)]
                 
                 (if (= (count subtree) 1)
                   (apply + my-path) ; Too much info: Instead of a (partial) path,  pass the (partial) sum only.
                   (let [sub-subtree (rest subtree)]
                     (min
                       (sub-paths sub-subtree col my-path)
                       (sub-paths sub-subtree (inc col) my-path))))))]
       
       (sub-paths tree 0 []))) ;the start tippoint is only one - no sibling
   '([1]
     [2 4]
     [5 1 4]
     [2 3 4 5])))
; others
(fn [t]
  (first
    (reduce
      (fn [a b]
        (map #(+ %1 (min %2 %3)) b a (rest a)))
      (reverse t))))
(fn [s]
    (first
     (reduce
      #(map + (map min (butlast %1) (rest %1)) %2) ;<<<< butlast
      (reverse s))))

(fn min-path
  ([rows] (min-path rows 0))
  ([rows i] (if-let [[row & rows] rows] ;WOW
              (+ (row i)
                 (min (min-path rows i)
                      (min-path rows (inc i))))
              0)))
 
;let and if-let handle destructuring of nil differently: 
(assert (= (let [[one & more]     []] [one more])  [nil nil]))
(assert (= (let [[one & more]    nil] [one more]) [nil nil]))
(assert (= (if-let [[one & more] [] ] [one more])  [nil nil]))
(assert (= (if-let [[one & more] nil] [one more])        nil)) ;!!
(if-let [[one & more] nil] [one more])

(letfn [(f [stop]
          (if stop
            true
            (recur (not stop))))] ;letfn can (recur)
  (f false))

;http://www.4clojure.com/problem/80 perfect numbers
(if false
  ((fn [n]
     (loop [i 1 sum 0]
       (if (= i n)
         (= sum n)
         (recur
           (inc i)
           (+ sum
              (if (= (mod n i) 0)
                i
                0))))))
   8128))
;others
(fn [x]
  (= x (reduce + (filter #(= 0 (mod x %)) (range 1 x))))) ;use (zero?)
(fn [n]
    (= n (apply + (filter #(zero? (mod n %)) (range 1 n))))) ;apply + instead of reduce +
(fn perfect? [n]
  (->> n
       (range 1)
       (filter
         #(zero? (mod n %)))
       (apply +)
       (= n)))
;??? TODO
(fn [x]
  (->> (range 1 x)
       (filter #(zero? (mod x %)))
       (apply +)
       (= x)))

;http://www.4clojure.com/problem/81 intersection
(if false
  ((fn [a b]
     (set
       (for [x a :when (b x)] x)))
   #{:a :b :c :d} #{:c :e :a :f :d}))
;others
#(set (filter % %2))
#(set (filter identity (map %1 %2)))
clojure.set/select



