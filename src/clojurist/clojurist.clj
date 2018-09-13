;TODO Learn how to make it a macro - how to access values of a, b instead of their source code
(defn assert= "Assert equality"
  ([a b] (assert (= a b) (str "a: " a ", b: " b)))
  ([a b msg] (assert (= a b) msg)))

; structures
(assert (= #{2 1 4 3} (sorted-set 2 1 4 3))) ;beware sorted and hash sets with same items are equal!
(assert (= (list 1 2) '(1 2))) ;similar to vector
(assert (= (#{1 2} 1) 1))
(assert= (range 1 3) '(1 2))
(assert (= [1 2] '(1 2)))

(defn modulo-four-comparator "compare after modulo 4" [a b] (< (mod a 4) (mod b 4)))
(= (sorted-set) #{}) ;true! Sets equal by items, regardless of comparator.
(= (sorted-set-by modulo-four-comparator) #{}) ;true!
(assert (= (conj (sorted-set-by modulo-four-comparator) 1 4 5) #{4 1}))

(type (conj  {})) ;check for # with {...} literals.
(type (conj #{}))

(= {} #{}) ;false is confusing if you missmatch types
(.getSuperclass (class {}))

(instance? clojure.lang.ISeq []) ;false
;(iterate #(.getSuperclass %) (.getClass '(1))) ;get all classes, but it fails after reaching Object


; TODO pass "expected" result(s), handle and undo them in (try (finally)).
; or (bind..) but for other threads: the macro creates an (anonymous) function and calls it
; -- but how to apply it to deeper but indirect macro-like calls?
; Can't assign macros to variables.
; (def ^:dynamic *g* 1) (binding [*g* 2] ...)
; https://clojure.org/reference/vars: var (def), atom, ref (if you need transactions) or agent (for async change).
; InheritableThreadLocal. childValue
; API design: io! macro: If the first expression in body is a literal string, will use that as the exception message. deref macro: timeout-ms timeout-val: return  timeout-val if the timeout is reached.
; transactions: dosync, alter, commute
; https://clojure.org/reference/agents and atoms: co-ordination, synchronisity
; add-watch: Var watchers are triggered only by root binding changes, not thread-local set!s.
; TODO (macroexpand ..)
(defmacro orExplain
    ([result] (orExplain :no_explanation))
    ([result explanation]
     (or result (assert false explanation))))


; TODO as a macro? Either way: Print the Clojure source and/or line # in assert.
; TODO sequences and vectors to be equal
; TODO clojure.lang.LongRange, clojure.lang.Range; Seq in general
(defn =typed
  "Compare two values as with =, but also checking their type compatibility.
  'options' can incude :order for differentiating between ordered and unordered
  collections or maps, :strict for exact type match (not advised for collections/sequences),
  :explain to assert failure with an explanation why the result would be false.
  This is NOT a subrelation of =, because Clojure's = treats lists and vectors as comparable."
  [a b & options]
  (and
    (orExplain (= a b) "Values not equal.")
    (or
       (= (type a) (type b)
        (let [optionsVec (apply vector options)
              bothAreMaps (and (instance? clojure.lang.APersistentMap a)
                               (instance? clojure.lang.APersistentMap b))
              bothAreSets (and (instance? clojure.lang.APersistentSet a)
                               (instance? clojure.lang.APersistentSet b))
              bothAreVectors (and (instance? clojure.lang.PersistentVector a)
                                  (instance? clojure.lang.PersistentVector b))
              bothAreSequences (and (instance? clojure.lang.PersistentList a)
                                    (instance? clojure.lang.PersistentList b))]
             
          (or
            (contains? optionsVec :strict)
           ; unordered and ordered maps and sets are equal, respectively, unless :ordered or :strict.
           ; Following uses classes/interfaces that are parent to both unordered and ordered versions
            (and
                 (not)
                 (not (and (instance? clojure.lang.APersistentSet a)
                           (instance? clojure.lang.APersistentSet b)))
                 (not (and (instance? clojure.lang.PersistentVector a)
                           (instance? clojure.lang.PersistentList))))))))
    (do
      (assert false (Exception. (str "Types differ: " (type a) (type b))))
      false)))
    
  

; TODO assert that it fails: (assert (not (=typed {} #{})))
; TODO ---- check *assert* and catch AssertionError only
(isa? (.getClass {}) Object)
(isa? (.getClass {}) clojure.lang.PersistentArrayMap)
(assert (instance? clojure.lang.APersistentMap {}))
(assert (instance? clojure.lang.APersistentMap (sorted-map)))
(assert (instance? clojure.lang.APersistentSet #{}))
(assert (instance? clojure.lang.APersistentSet (sorted-set)))
(assert (instance? clojure.lang.ArraySeq ( (fn [& params] params) :any-non-empty)))
;(extends? (Object.) Object)

(defn for-all "Combine items from each given sequence." [seq & more]
 (loop [combinations '(()) seq seq more more]
   (if seq
     (recur (for [one seq combined combinations] (cons one combined))
       (first more)
       (next more))
     #_else combinations)))

(for-all [1 2] [true false])
(for-all [1 2] [true false] '(:a :b))

(for [a '(true false) b '(true false)]
  (assert=
    (not (and a b))
    (or (not a) (not b))))

(conj {:i 1} [:k 3] [:l 4])

(get #{nil} nil)
(contains? #{nil} nil)
(assert (= ({nil 1} nil) 1))

(map (fn [& _] true) #{1 2 3})
(filter #(do [%] true) [:anything :here])

;(map (fn [] true) #{1 2 3})
;(filter #(do true) [:anything :here])
[1 #_anything :back-to-evaluating] ; EDN discard
[1 #_ [2 ANY [INNER within the "block"]] :back-to-evaluating] ; EDN discard

;TODO
(if false
  (do
   (let [fact (fn [n] (
                          (loop [n n product 1] (if (<= n 1) product (recur (dec n) (* n product))))))]  (fact 4))

   (let [fact (fn [n] (
                          (loop [n n product 1]
                            (if (<= n 1)
                              product
                              (recur (dec n) (* n product))))))]  (fact 4))))



(let [fact (fn [n] ;Don't have an extra ( here. Otherwise you'll get: Long can't be cast to class clojure.lang.IFn!
               (loop [n n product 1]
                 (if (<= n 1)
                   (do (println product) product)
                   (do (println (str "n " n ", product " product)) (recur (dec n) (* n product))))))] (fact 4))

( (fn [x] {:post [(= % x)]} x) :anything) ;=> :anything - OK
;( (fn [x] {:post [(= % x)]} :incorrect) :anything) ;=> nothing shown as a result in InstaREPL
*assert* ;=> true

(assert (= (int 0.6) 0)); round down!

; comp
( (comp :street :address) {:address {:street "1 Main"}})
(let [address-part (fn [address-field-keyword]
                     (comp address-field-keyword :address))]
  ( (address-part :street) {:address {:street "1 Main"}}))

((comp int inc #(/ % 2)) 10)
( (fn [text]
    (reduce
      #_applier_of_each_function_from_vector (fn [text transformation] (transformation text))
      text
      #_vector_of_transformations [clojure.string/upper-case #(str "Hi " %)]))
 "john")

(= identity (comp)) ; identity accepts one parameter only!

(defn my-reverse [coll]
  (loop [coll coll result (empty coll)]
    (if (not-empty coll)
      (recur (pop coll) (conj result (peek coll)))
      result)))
(= (my-reverse [1 2 3 4]) [4 3 2 1])
(= (my-reverse []) [])
(= (my-reverse '(1 2 3 4)) '(4 3 2 1))
(= (my-reverse '()) '())

(defn my-comp
  ;[] (do identity)
  [& functions]
  (fn [& params]
    (loop [reversed-functions (reverse functions)
           params params]
      (let [func (peek reversed-functions)]
        (if func
          (recur (pop reversed-functions) (conj '() (apply func params)))
          #_apply_expects_params_to_be_a_sequence_but_previous_functions_return_one_value
          (first params))))))

(assert (= ((my-comp :street :address) {:address {:street "1 Main"}}) "1 Main"))

( (fn [& params] params) :hi); [...] & params] captures into a sequence, not a collection

(defn loop-access-outer [par]
  (loop [i 5]
    (if (> i 0)
      (recur (dec i))
      par)))
(loop-access-outer :hi)     
    
(def varr 1)
(type #'varr) ; Var
(let [x 1] (type #'x)) ;(let) doesn't define "Var" but a symbol

(assert= (let [x 0] (let [x (inc x)] x)) 1) ;(let) can re-bind an existing symbol

(into [] '(1 2)) ; sequence into vector
(apply vector '(1 2)) ;another way
(vec '(:a :b :c))
(vector :a :b :c)

(let [veccy [1 2 3]] (let [ [& sequ] veccy] sequ)); Vector into sequence
(seq [1 2 3]) ; Vector into sequence - easy
(list 1 2 3);

(let [[one & others] '[1 2 3]] others) ;Like & rest-param in functions, destructuring a vector in & rest-param via (let [[.. & rest-param]]) makes the rest a sequence!

(-> "john" (clojure.string/capitalize) (#(str "Hi " %))) ;Threading macro but into a second/further parameter: via anonymous function #().
(->> "john" (clojure.string/capitalize) (str "Hi "));

(map (fn [name age] {:name name :age age}) ["John" "Luke"] '(19 23))
({:name "John", :age 19} {:name "Luke", :age 23}) ;(map) accepts both vectors and sequences

(conj {:a 1} {:b 2}) ;(conj) to a map accepts both map(s) or vector(s) of pairs
(conj {:a 1} [:b 2])

( (partial + 1 2) 4)
( #(nth % (dec (count %))) [1 2 3]) ;same as (last [1 2 3])
( #(if (= %2 0) (first %1) (recur (rest %1) (dec %2))) [1 2 3 4] 2) ; same as (nth [1 2 3 4] 2)
( #(loop [s % C 0] (if (= s []) C (recur (rest s) (inc C)))) '(1 2 3)) ; alternative to (count '(1 2 3))
#(apply + (map (fn [_] 1) %)) ;short alt. to count

(rseq [1 2]) ; same as reverse; both return sequences
(= (rseq (sorted-map :i 1 :j 2)) '([:j 2] [:i 1])) ;reverse or rseq of a map returns a sequence of map entries (each a vector) in reverse order

( #(loop [in % out '()]
     (if (not (empty? in))
       (recur (rest in) (cons (first in) out))
       out))
       
 [1 2 3 4]) ;alt. to reverse; rest can return an empty sequence, but next can return nil. (cons x seq) but (conj coll x...)

;
( (fn [s]
    (map (vec s) ;use the vector as a function
      (range (dec (count s)) -1 -1))) ;reverse range
 [1 2 3])
(
 (fn [s]
  (reduce conj '() s)) ;short reverse
 '(1 2 3))

(#(reduce + 0 %) '(1 2 3)) ;sum up
(reduce + [1 2 3]) ;sum up

( #(filter odd? %) [0 1 2 3])


; Fibonacci 8: '(1 1 2 3 5 8 13 21)
( (fn [N]
    (case N
      [1] 1
      [2] [1 1]
      (loop [so-far [1 1] n 3 N N]
         (if (<= n N)
           (recur (conj so-far (+ (so-far (- n 3)) (so-far (- n 2)))) (inc n) N)
           so-far)))) 8)
( #(let [v (vec %)] (= v (reverse v))) "abcba") ;palindrome?  http://www.4clojure.com/problem/27    
       
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
 [1 2 [3 [4] '(5)]]) ;http://www.4clojure.com/problem/28
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
(reduce #(conj %1 (vector %2 %2)) [] [1 2 3])
 
   
  
   
   
  
      
    
    
    
    
  
  
          
          
      
      
    
    
    
















