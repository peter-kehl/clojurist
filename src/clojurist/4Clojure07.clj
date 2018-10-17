(require 'clojure.set)
(clojure.main/load-script "/home/pkehl/GIT/clojurist/src/clojurist/dbg.clj")

; http://www.4clojure.com/problem/102 intoCamelCase
; --https://lispcast.com/clojure-regex/
(fn [s]
  (clojure.string/replace s #"(-)([a-z])"
    (fn [[_ _ l]] (clojure.string/upper-case l))))

; http://www.4clojure.com/problem/115 digits balance
(def balance
 (fn [n] 
   (let [n-seq (seq (str n))
         length (count n-seq)
         half (int (/ length 2))
         one (take half n-seq) ;number first, so it can also return a transducer.
         two (nthrest n-seq (if (odd? length) (inc half) half)) ;Reverse order to that of (take)!
         ; the following would work without detracting 48 (which is (int \)), too.
         digit-sum (fn [digs] (println digs) (apply + (map #(- (int %) 48) digs)))]
     (= (digit-sum one) (digit-sum two)))))
(if false
  (balance 11))   
;others
(fn [x]
  (let [s (str x)
        n (count s)
        [a c] (split-at (quot n 2) s)
        b (if (odd? n) (next c) c)
        f #(reduce + (map int %))]
    (= (f a) (f b))))
(fn [n]
    (let [s (str n), l (count s)]
      (= (reduce + (map int (take (Math/ceil (/ l 2)) s)))
         (reduce + (map int (drop (Math/floor (/ l 2)) s))))))
(fn [n]
  (let [s (str n)
        m (quot (count s) 2)
        ds #(apply + (map (comp read-string str) (take m %)))]
    (= (ds s) (ds (reverse s))))) ;<<<<<
#(let [xs (map int (str %))
       n (/ (count xs) 2)]
   (= (apply + (take n xs))
      (apply + (take-last n xs)))) ;<<<

;http://www.4clojure.com/problem/105 identify keywords & 'values'
(fn [seque]
  (loop [res {}
         seque seque]
    (if (empty? seque)
      res
      (let [keyw (first seque)
            values (take-while
                         #(not (keyword? %))
                         (rest seque))
            leftover (drop (inc (count values)) seque)
            conjoined (conj res [keyw values])]
           (recur conjoined leftover)))))
       
  
;http://www.4clojure.com/problem/84 transitive closure
(def transit
  (fn [pairs]
      (let [one2many
            ;could have replaced with (reduce {} ...)
            (loop [one2many {} ;from item => #{to item...} for all transient closures from 'from item' so far
                   more pairs]
              (let [[from to :as pair] (first more)
                    ; abc2xyz are sets; from and to as per above;
                    ; orig means an applicable subset of original source/target items
                    ; new means a (potentially) new subset of source/target items
                    from2orig (get one2many from #{})
                    one2manyPlusDirect (conj one2many [from (conj from2orig to)])
                    
                    to2orig   (get one2many to #{})
                    
                    from2new (conj to2orig to) ;new items that will be connected from 'from' and from all items already connected to 'from'
                    
                    ;_ (dbg-println :from from :to to :from2orig from2orig :one2manyPlusDirect one2manyPlusDirect :to2orig to2orig :from2new from2new)
                    one2manyUpdated (into {}
                                      (map
                                        (fn [[source targets]]
                                          ;(println :source source :targets targets)
                                          [source
                                           (if (or
                                                   (= source from)
                                                   (contains? targets from))
                                             (clojure.set/union targets from2new)
                                             targets)])
                                        one2manyPlusDirect))
                    others (next more)]
                (if others
                  (recur one2manyUpdated others)
                  one2manyUpdated)))]
        (println :one2many one2many)
        (reduce
          (fn [res [from targets]]
            ;(dbg-println :reduce-> res from targets)
            (let [joined
                  (apply conj
                    res
                    (map
                      #(vector from %)
                      targets))]
              (println :joined joined) 
              joined))
          #{}
          one2many))))
(if false
     (transit
       #{["cat" "man"] ["man" "snake"] ["spider" "cat"]}))
(if false
  (=
     (transit
       #{["cat" "man"] ["man" "snake"] ["spider" "cat"]})
     #{["cat" "man"] ["cat" "snake"] ["man" "snake"]
       ["spider" "cat"] ["spider" "man"] ["spider" "snake"]}))

; #{[8 4] [9 3] [4 2] [27 9]} is stored in order: #{[27 9] [9 3] [8 4] [4 2]}
(transit #{[8 4] [9 3] [4 2] [27 9]})

(if false
  (let [divides #{[8 4] [9 3] [4 2] [27 9]}]
    (= (transit divides) #{[4 2] [8 4] [8 2] [9 3] [27 9] [27 3]})))








































