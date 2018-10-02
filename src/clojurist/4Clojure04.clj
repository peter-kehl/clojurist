(require 'clojure.set)
(require 'clojure.pprint)

;http://www.4clojure.com/problem/89 graph tour           
;can visit every edge exactly once =
;all edges are within the same graph with no islands & (or (all nodes have an even number of edges) (exactly two nodes have uneven umber of edges))
;allowing multiple direct edges between any two nodes. Allowing self-pointing edge(s) [x x].
#_
(no.3
 a b c d
 ---
 ---
 -----
 -----
 -------
   -----
      --
 no.5 two closed loops
 a b c d e f
 ---
 -----
   ---
 ---------
   -------
 -------
   -----
     -----
       ---
     -------
       -----)
(def graph-tour
  (fn [gr]
    (let [n2e2num (reduce ;n2e2num will be a map: node => edge pair that is [node node] => number of such edges
                    (fn [result edge]
                      
                      (let [from (first edge)
                            to (last edge)
                            edges-from (result from {})
                            edges-to   (result to {})
                            edge-reverse (reverse edge)
                            num-from     (edges-from edge 0)
                            num-to       (edges-to   edge-reverse 0)
                            edges-from-new  (conj edges-from [edge         (inc num-from)])
                            edges-to-new    (conj edges-to   [edge-reverse (inc num-to)])]
                        (conj result
                          [from edges-from-new]
                          [to   edges-to-new]))) ; (reverse edge) is a seq, not a vec, but that's OK when comparing later
                    {}
                    gr)
          ;_ (clojure.pprint/pprint n2e2num)
          n2n2num (into {}
                    (map
                      (fn [[from edge2num]]
                        [from (reduce
                                (fn [res [[e-from e-to] num]]
                                  (do
                                    (assert (= e-from from) (str "e-from: " e-from ", but from: " from))
                                    (let [old (res e-to 0)]
                                      (conj res [e-to (+ old num)]))))
                                {}
                                edge2num)])
                      n2e2num))
          ;_ (clojure.pprint/pprint n2n2num)
          connected-nodes (loop [res #{} step #{(first (first n2n2num))}]
                            (let [res-new (clojure.set/union res step)]
                              (if (= res res-new)
                                res
                                (let [step-new (set
                                                 (for [ [from n2num] n2n2num
                                                       :when (step from)
                                                       [to _] n2num]
                                                   to))] ;<<< (for) allows to flatten multi-dimensional
                                  
                                  (recur res-new step-new)))))
          ;_ (clojure.pprint/pprint connected-nodes)
          all-nodes-connect (= (count connected-nodes) (count n2n2num))
          ;_ (clojure.pprint/pprint all-nodes-connect)
          sum-edges-per-node-vals (for [[_from e2num] n2e2num] ;same edge can have multiple occurrences
                                    (apply + (vals e2num)))
          ;_ (clojure.pprint/pprint sum-edges-per-node-vals)
          
          num-nodes-with-even-odd-connections (let [by-mod2 (group-by
                                                              #(mod % 2)
                                                              sum-edges-per-node-vals)]
                                                [ (count (by-mod2 0)) (count (by-mod2 1))])]
          ;_ (clojure.pprint/pprint num-nodes-with-even-odd-connections)]
      
      (and
           all-nodes-connect
           (or
               (zero? (num-nodes-with-even-odd-connections 1))
               (= (num-nodes-with-even-odd-connections 1) 2))))))
(if false
  (graph-tour [[:a :b]]))
;(println "no 3")
(if false ;odd :a :b :c :d, even none
  (graph-tour [[:a :b] [:a :b] [:a :c] [:c :a]
               [:a :d] [:b :d] [:c :d]]))

(if false
  (graph-tour [[1 2] [2 3] [3 4] [4 1]]))
;(println "no 5")
(if false
  (graph-tour [[:a :b] [:a :c] [:c :b] [:a :e]
               [:b :e] [:a :d] [:b :d] [:c :e]
               [:d :e] [:c :f] [:d :f]]))
; for the above
(= '(1) [1])
({(list 1) :x} [1])
;others
(fn [gs]
  (let [U (comp set concat)
        g (apply merge-with U
                 (concat
                   (map (fn [[a b]] {a #{b}}) gs)
                   (map (fn [[a b]] {b #{a}}) gs)))
        k (set (keys g))]
    (boolean
      (and
        (->> gs
          (mapcat (fn [[a b]] [[a b] [b a]]))
          (group-by first)
          (map val)
          (map count)
          (remove even?)
          (count)
          (#{0 2}))
        (let [f (fn f [s v]
                  (or
                      (empty? v)
                      (some identity (map #(when (v %) (f % (disj v %))) (g s)))))]
          (some identity (map #(f % (disj k %)) k)))))))

;geekerzp:
(fn [g]
    (letfn [(d [g]
              (apply merge-with + {} (for [[a b] g
                                           :when (not= a b)]
                                       {a 1 b 1})))]
      (and
       (not (empty? (d g)))
       (->> (vals (d g)) (filter odd?) count (>= 2)))))
;lambda4fun:
(fn [edges]
  (let [nodes (->> edges
                   (map (fn [[a b]] (merge {a #{b}} {b #{a}})))
                   (apply merge-with clojure.set/union))
        passed (atom #{})]
    (letfn [(go [[from to :as edge] n]
              (when-not (@passed (set edge))
                (or (= n (count edges))
                    (do (swap! passed conj (set edge))
                        (some #(go [to %] (inc n)) (nodes to))))))]
      (->> (for [edge edges]
             (do (reset! passed #{}) ;<<<<TODO
                 (go edge 1)))
           (some identity)
           boolean))))
;jarlax:
(fn [edges]
  (let [updated (fn [dict key val]
                  (conj (dict key '()) val))
        add-edge (fn [adj [from to]]
                   (into adj [[from (updated adj from to)]
                              [to (updated adj to from)]]))
        adj (reduce add-edge {} edges)
        start (first (first edges))
        desired (count (keys adj))
        neighbors (fn [v vis] (clojure.set/difference (set (adj v)) vis))
        connected? (loop [[x & _ :as all] (list start) vis #{}]
                    (cond
                      (empty? all) (= (count vis) desired)
                      :else (recur
                              (concat (rest all) (neighbors x vis))
                              (conj vis x))))
        satisfies? (->> adj
                        vals
                        (map count)
                        (filter odd?)
                        count
                        (> 3))]
    (and satisfies? connected?)))
;kohyama:
(fn k [s]
  (letfn [
          (rem [x coll] ;beware that rem is a core function, here hidden!
            ((fn [[a b]] (concat a (next b)))
             (split-with #(not= % x) coll)))
          (paths [[[p q :as prev] :as path] rests]
            (if (empty? rests)
              [path]
              (apply concat
                (keep
                  (fn [[r s :as x]]
                    (cond (nil? prev) ;Nightcode Paredit mode requires condition - result to be either on the same line, or with the same indentation!
                          (concat
                            (paths (cons [r s] path) (rem x rests))
                            (paths (cons [s r] path) (rem x rests)))
                      (= q s)
                      (paths (cons [s r] path) (rem x rests))
                      (= q r)
                      (paths (cons [r s] path) (rem x rests))))
                  rests))))]
    (boolean
      (some
        #(= (count %) (count s))
        (paths () s)))))

;http://www.4clojure.com/problem/90 Cartesian product
(fn [one two]
  (set
    (for [o one
          t two]
      [o t])))
;others
(fn [s1 s2]
   (into #{} (for [s1 s1
                   s2 s2]
               [s1 s2])))
(fn [a b]
  (into #{}
    (mapcat (fn [x] (map #(vector % x) a)) b)))   
  
;http://www.4clojure.com/problem/91 graph connectivity
(def graph-connectivity ;TODO
  (fn [graph]
    (let
      [n2nodes (reduce
                (fn [n2nodes [from to]]
                  ; if there were any self-pointing edges [x x], then we'd have to skip them, as that would corrupt the count
                  (let [connected (fn [hub]
                                    (n2nodes hub #{}))
                        ;from2nodes (connected from)
                        ;to2nodes   (connected to)
                        
                        connected-add (fn [hub add]
                                        (conj (connected hub) add))
                        ;from2nodes-new (conj from2nodes to)
                        ;to2nodes-new   (conj to2nodes from)
                        
                        extend-connected (fn [hub add] ;take all nodes connected to hub; connect each with add
                                           (reduce
                                             (fn [res hub-item]
                                               (conj res [hub-item (connected-add hub-item add)]))
                                             {}
                                             (connected hub)))]
                    
                    ;(conj n2nodes [from from2nodes-new] [to to2nodes-new])
                    (clojure.pprint/pprint n2nodes)
                    (println "From" from "To" to)
                    (clojure.pprint/pprint (connected-add from to))
                    (clojure.pprint/pprint (connected-add to from))
                    (clojure.pprint/pprint (extend-connected from to))
                    (clojure.pprint/pprint (extend-connected to from))
                    (println)
                    (merge
                      (conj n2nodes
                        [from (connected-add from to)]
                        [to   (connected-add to   from)])
                      (extend-connected from to)
                      (extend-connected to   from)))
                  {}
                  graph))]
      (clojure.pprint/pprint n2nodes)
      (= (count (first n2nodes)) ; num. of (de-duplicated) connections from the 1st (arbitrary) node
         (dec (count n2nodes))))))
    
(def graph-connectivity
  (fn [graph]
    (let [islands (reduce
                    (fn [islands [from to]]
                      (let [find-island (fn [node]
                                          (into #{} ;into ensures the result is non-nil (i.e. potentically an empty set)
                                            (first
                                              (doto
                                                (for [island islands :when (island node)]
                                                     island)
                                                #(assert (<= (count %) 1))))))
                            island-from (find-island from)
                            island-to   (find-island to)]
                          
                        (conj (disj islands island-from island-to) ;empty (new) islands and duplicate edges are OK
                          (into island-from
                            (conj island-to from to)))))
                    #{}
                    graph)]
      (= (count islands) 1))))

(if false
  (graph-connectivity #{[1 2] [2 3] [3 1]
                        [4 5] [5 6] [6 4]}))
;(println "----")
(if false
  (graph-connectivity #{[1 2] [2 3] [3 1]
                        [4 5] [5 6] [6 4] [3 4]}))

(assert (= nil nil))
(assert (= (into {:i 1} {:i 2}) {:i 2})) ;into honours the later entries
;others
(fn [t]
  (let [g (reduce (fn [z [a b]] (merge-with concat z {a [b]} {b [a]})) {} t)
        h (iterate #(into % (mapcat g %)) #{(ffirst t)})
        [[a _] & _] (drop-while (fn [[a b]] (not= a b)) (map vector h (next h)))]
    (== (count (keys g)) (count a))))
;geekerzp
(fn [s]
    (apply = (vals (reduce
                    (fn [g [a b]]
                      (let [r (clojure.set/union (g a #{a}) (g b #{b}))]
                        (reduce #(assoc % %2 r) g r)))
                    {} s))))
;lambda4fun
(fn [edges]
  (let [nodes (->> edges
                   (map (fn [[a b]] (merge {a [b]} {b [a]})))
                   (apply merge-with concat))
        passed (atom #{})]
    (letfn [(go [node-id]
              (when-not (@passed node-id)
                (swap! passed conj node-id)
                (doseq [node-id' (nodes node-id)]
                  (go node-id'))))]
      (go (key (first nodes)))
      (= (count @passed)
         (count nodes)))))


;Paredit can't think for you. If you re-indent one of the branches below, it will change parenthesis yet still compile & run, but they won't be equal.
(assert (=
           (+ 1
              (* 2 3)
              4)
           (+ 1
              (* 2 3)
              4))) 
;http://www.4clojure.com/problem/92 Roman numerals
(def roman
  (fn [in-str]
    (let [value2char (sorted-map 1 \I 5 \V 10 \X 50 \L 100 \C 500 \D 1000 \M)
          char2value (into {} (map (comp vec reverse) value2char))]
      ;(clojure.pprint/pprint char2value)
      
      (loop [chars (vec in-str)
             sum-so-far 0
             last-digit \0
             last-digit-occurrences 0]
        (println "sum-so-far" sum-so-far "last-digit" last-digit "last-d-oc" last-digit-occurrences)
        (if (empty? chars)
          (+ sum-so-far (* (char2value last-digit) last-digit-occurrences))
          (let [digits (for [c chars :while (= c (first chars))] c)
                digit (first digits)
                digit-occurrences (count digits)
                digit-value (char2value digit)
                last-digit-value (char2value last-digit 0)
                digit-standard-order (or (= last-digit \0) ;don't use zero? because it throws on non-number
                                         (> last-digit-value digit-value))]
            (assert (or
                        digit-standard-order
                        (and (or (= last-digit-value (/ digit-value 5))
                                 (= last-digit-value (/ digit-value 10)))
                             (= digit-occurrences last-digit-occurrences 1))))   
            
            (recur
              (drop digit-occurrences chars)
              (+ sum-so-far
                 (if digit-standard-order
                   (* last-digit-value last-digit-occurrences)
                   (- digit-value last-digit-value)))
              digit
              (if digit-standard-order
                digit-occurrences
                0))))))))  
      
(if false
  (roman "XIV"))
;(println "---- after my Roman")
; Can't (into {} (map reverse {:i 1 :j 2})), because (reverse) doesn't return Map.Entry
(into {} (map (comp vec reverse) {:i 1 :j 2})) ; (into {} ...) requires the entry pairs to be vector(s), not sequence(s)
;zero? is not exactly the same as = 0. Why? zero? requires the parameter to be a number. (zero? \a) or (zero? nil) throws an exception, but (= \a 0) returns false. 
;others
(fn [n]
  (reduce
    (fn [a d]
      (if (< (* 3 d) a) (- a d) (+ d a)))
    (map 
     {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1} 
     (reverse n))))
(fn [s]
  (let [roman {"M" 1000 "CM" 900 "D" 500 "CD" 400}] ;listing the combination = manual but it saves programming time
    "C" 100 "XC" 90 "L" 50 "XL" 40 "X" 10 "IX" 9
    "V" 5 "IV" 4 "I" 1
    (reduce + (map roman)
      (re-seq #"CM|CD|XC|XL|IX|IV|[MDCLXVI]" s))))
(fn [rn]
  (letfn 
    [(tokenize [s]
      (-> s
        (clojure.string/replace "IV" " 4")
        (clojure.string/replace "IX" " 9")
        (clojure.string/replace "XL" " 40")
        (clojure.string/replace "XC" " 90")
        (clojure.string/replace "CD" " 400")
        (clojure.string/replace "CM" " 900"
          (clojure.string/replace "I"  " 1")
          (clojure.string/replace "V"  " 5")
          (clojure.string/replace "X"  " 10")
          (clojure.string/replace "L"  " 50")
          (clojure.string/replace "C"  " 100")
          (clojure.string/replace "D"  " 500")
          (clojure.string/replace "M"  " 1000"))))]
    (->> rn
         tokenize 
         (#(clojure.string/split % #" "))
         (remove empty?)
         (map #(Integer. %))
         (apply +))))
(fn [s]
    (let [roman {"M" 1000
                 "CM" 900
                 "D"  500
                 "CD" 400
                 "C"  100
                 "XC"  90
                 "L"   50
                 "XL"  40
                 "X"   10
                 "IX"   9
                 "V"    5
                 "IV"   4
                 "I"    1}]
      (reduce +
              (map roman
                   (re-seq #"CM|CD|XC|XL|IX|IV|[MDCLXVI]" s)))))
(fn [roman]
  (reduce
    (fn [total [num next]] ((if (< num next) - +) total num))
    0
    (partition 2 1 (concat (map {\M 1000 \D 500 \C 100 \L 50 \X 10 \V 5 \I 1} roman) '(0)))))
#(let [m1 {"IV" 4 "IX" 9 "XL" 40 "XC" 90 "CD" 400 "CM" 900}
       m2 {\I 1 \V 5 \X 10 \L 50 \C 100 \D 500 \M 1000}]
   (loop [s % sum 0]
     (if (seq s)
       (if-let [x (m1 (apply str (take 2 s)))]
         (recur (drop 2 s) (+ sum x))
         (recur (rest s) (+ sum (m2 (first s)))))
       sum)))

;http://www.4clojure.com/problem/93 flatten but maintains the lowest level sequential items.
;no need for a universal solution for e.g. [1 [2 [3]]]
(def partial-flatten
  (fn part-flat [items]
     (for [i items
           j (if (some sequential? i)
               (part-flat i)
               [i])]
       j)))
       
(if true
  ( partial-flatten [[[[:a :b]]] [[:c :d]] [:e :f]])) 
;seq? is for sequence only; sequential? includes vectors
;others
(letfn [(t [a] (and (sequential? a) (every? sequential? a)))]
  (fn f [s]
    (if (t s)
      (mapcat #(if (t %) % [%]) (map f s))
      s)))
(fn [s]
  (filter #(and (sequential? %) (not (sequential? (first %))))
    (tree-seq sequential? seq s)))
#(remove (partial every? coll?) (tree-seq (partial every? coll?) identity %))
(fn [x]
  (filter #(every? (complement sequential?) %)
    (filter sequential?
      (rest (tree-seq sequential? seq x))))) ;<<<< depth-first
(fn [coll]
  (filter
    (complement #(every? sequential? %)) 
    (tree-seq #(every? sequential? %) seq coll)))
(fn pf [coll]
  (mapcat
    #(if (coll? (first %))
         (pf %)
         (list %))
    coll))

(fn flat [xs]
  (if (coll? xs)
    (if-let [[x & xs] xs]
      (concat (if (some coll? x)
                (flat x)
                [x])
              (flat xs)))
    xs))
;(println "---- end of partial flatten")

; http://www.4clojure.com/problem/94 game of life
(def game-life
  (fn [mx]
    (let [height (count mx)
          width (count (first mx))
          
          exists (fn [row col]
                   (and (>= row 0) (< row height)
                        (>= col 0) (< col width)))
          alive (fn [row col]
                  (= (get (mx row) col) \#)) ;mx is a vector of strings, even though game-life returns a seq of strings
          num-neighbs-alive (fn [row col]
                               (count
                                 (for [r-delta '(-1 0 1) c-delta '(-1 0 1)
                                       :let [r (+ row r-delta) c (+ col c-delta)]
                                       :when (and (exists r c)
                                                  (not= r-delta c-delta 0)
                                                  (alive r c))]
                                   1)))
          alive-next (fn [row col]
                       (let [neighbs (num-neighbs-alive row col)]
                         ;(print neighbs)
                         ;(if (= col (dec width)) (println))
                         (if (alive row col)
                           (or (= neighbs 2) (= neighbs 3))
                           (= neighbs 3))))]
      (for [row (range 0 height)]
        (apply str
          (for [col (range 0 width)]
            (if (alive-next row col)
              \#
              \space)))))))
;(println "game of life")                

(if true 
  (game-life
           ["      "  
            " ##   "
            " ##   "
            "   ## "
            "   ## "
            "      "]))
;(println "after game of life")                
;others
(fn [d]
    (for [i (range (count d))]
      (apply
       str
       (for [j (range (count (d i)))]
         (let [z (= \# (get-in d [i j])) ;<< get-in coll seq-of-keys
               v [-1 0 1]
               u (count (filter #(= \# (get-in d %)) (for [a v, b v] [(+ i a) (+ j b)])))]
           (if (or (== 3 u) (and z (== 4 u))) \# " "))))))
(fn [board]
  (letfn [
          (row-to-ints     [row] (map #(if (= \# %) 1 0) row))
          (row-to-bools    [row] (map #(= \# %) row))
          (shift-row-right [row] (cons 0 row))
          (shift-row-left  [row] (conj (vec (rest row)) 0))
          (shift-board-up   [rows] (conj (vec (rest rows)) (map (partial * 0) (first rows))))
          (shift-board-down [rows] (cons (map (partial * 0) (first rows)) rows))
          (add-boards [a b] (map (partial map +) a b))
          (count-neighbors [board]
            (reduce add-boards 
              [ (shift-board-up board)
               (shift-board-down board)
               (map shift-row-right board)
               (map shift-row-left board)
               (shift-board-up (map shift-row-right board))
               (shift-board-up (map shift-row-left board))
               (shift-board-down (map shift-row-right board))
               (shift-board-down (map shift-row-left board))]))
          (simulate [cell neighbor-count]
            (cond
              (and cell (< neighbor-count 2)) false
              (and cell (< neighbor-count 4)) true
              (and (not cell) (= neighbor-count 3)) true
              :else false))
          (print-board [board]
            (map (fn [row] (apply str (map #(if % \# " ") row))) board))]
    (print-board
      (map (partial map simulate)      
        (map row-to-bools board)
        (count-neighbors (map row-to-ints board))))))
(fn [b]
    (letfn [(n [r c]
              (for [i (range -1 2) ;<<< eq. [-1 0 1]
                    j (range -1 2)
                    :when (not= 0 i j)]
                (get-in b [(+ r i) (+ c j)])))]
      (let [r (count b) c (count (first b))]
        (->>(for [i (range r)
                  j (range c)]
              (let [l (count (filter #(= \# %) (n i j)))]
                (cond
                  (and (= l 2)
                       (= \# (get-in b [i j]))) \#
                  (= l 3) \#
                  :else \space)))
            (partition c) ;<<<<<<<<<<<<<<<<<<<<<<<<
            (map #(apply str %))))))
(fn [cells]
  (let [w (count cells)
        h (count (first cells))
        live? (fn [x y] (= \# (get-in cells [y x])))
        count-lives (fn [cx cy] (count (for [x [(dec cx) cx (inc cx)]
                                             y [(dec cy) cy (inc cy)]
                                             :when (live? x y)]
                                         1)))
        row (fn [y] (->> (range w)
                         (map (fn [x]
                                (let [num-lives (count-lives x y)]
                                  (if (live? x y)
                                    (if (#{3 4} num-lives) "#" " ")
                                    (if (#{3}   num-lives) "#" " ")))))
                         (apply str)))]
    (map row (range h))))

;http://www.4clojure.com/problem/95 bin tree?
(def is-bin-tree?
  (fn bin-tree? [node]
    (and (sequential? node)
         (= (count node) 3)
         (let [left (nth node 1)
               right (nth node 2)]
          (and
             (or (nil? left) (bin-tree? left))
             (or (nil? right)(bin-tree? right)))))))
;others
(fn f [s] 
  (if (sequential? s) 
    (let [[_ a b] s] ;<<<< destructuring => missing values become nil
      (and (= 3 (count s)) (f a) (f b)))
    (nil? s)))
(fn [r]
    (every?
     #(or
       (nil? %)
       (and (sequential? %) (= 3 (count %))))
     (tree-seq sequential? rest r))) ;<<<<<
(fn b-tree? [node]
   (or 
     (nil? node)
     ((every-pred ;<<<<<
        sequential?
        (comp (partial = 3) count)
        (comp b-tree? #(nth % 1))
        (comp b-tree? #(nth % 2))
       node))))
  
;http://www.4clojure.com/problem/96 symmetric tree?
(def is-sym-tree?
  (fn sym-root? [node] ;assume a well-formed binary tree
    (letfn [(sym-trees? [one two]
              (or
                  (= one two nil)
                  (and
                       (not (nil? one))
                       (not (nil? two))
                       (= (first one) (first two))
                       (sym-trees? (nth one 1) (nth two 2))
                       (sym-trees? (nth one 2) (nth two 1)))))]
      (sym-trees? (nth node 1) (nth node 2)))))
;others
#(
  (fn s [[a b c :as x] [k l m :as y]]
    (or 
      (not (or x y))
      (and (= a k) (s b m) (s c l)))) % %)
(fn [root]
  (letfn [(flip [node] (if (coll? node) [(first node) (flip (nth node 2)) (flip (nth node 1))] node))]
    (= (nth root 1) (flip (nth root 2))))) ;<<< rather than traverse & compare, transform to a form that can be compared by = standard
(fn [t] 
  (=
   (nth t 1) 
   ((fn flip [node] ;<<< the same as previous, but eliminating leftn. Define a recursive function right where you need it.
      (if (nil? node)
        nil 
        [(nth node 0) (flip (nth node 2)) (flip (nth node 1))]))
    (nth t 2))))
(fn [[v l r]] (= l)
  ((fn mirror [[v l r :as t]] ;<< :as
     (if (nil? t) nil [v (mirror r) (mirror l)]))
   r))
#(let [tree ((fn ! [[v left right :as node]]
                 (if (coll? node)
                   (concat (! left) [v] (! right))
                   [v])) 
             %)]
   (= tree (reverse tree))) ;<<< TODO

;http://www.4clojure.com/problem/97 Pascal's triangle
(def pascal
  (fn [n]
    (loop [row '(1)
           level 1]
      (if (= level n)
        row
        (recur
          (concat
            (cons 1 (for [i (range 0 (dec level))] ;reversed order range keeps standard step 1, which results in an empty range
                      (+ (nth row i) (nth row (inc i))))) ;<< nth fails on wrong index
            '(1))
          (inc level))))))     
(if false
  (map pascal (range 1 6))) 
;others
(fn [x] 
  (nth 
    (iterate #(concat [1] (map + % (rest %)) [1]) [1]) ;<<< iterate
    (dec x)))
(fn [r]
  (reduce ;<<<
    #(cons
       (* (first %1)
          (/ (- r %2) %2))
       %1)
    [1] (range 1 r)))
(fn [n]
    (last (take n (iterate #(map + (concat [0] %) (concat % [0])) [1]))))

;http://www.4clojure.com/problem/98 Equivalence classes
;partially similar to determining if a group of node edges is fully connected
(def eq-classes
  (fn [f D]
    (reduce
      (fn [result a]
        (if false (let [extended
                         (reduce
                           (fn [res b-set]
                             1)
                           result)]))
        (let [eq-set
              (some
                #(if (= (f a) (f (first %)))
                   %)
                result)]
          (if eq-set
            (conj
              (disj result eq-set)
              (conj eq-set a))
            (conj result #{a}))))
      #{}
      D)))
(if false
  (eq-classes #(* % %) #{-2 -1 0 1 2}))
;others
#(set (map set (vals (set (group-by % %2))))) ;<< group-by
(fn [f D] (set (map set (vals (group-by f D)))))
(fn [f s]
  (set (for [a s]
         (set (for [b s :when (= (f a) (f b))] b)))))
(fn [f s]
  (->> (group-by f s)
       (map second)
       (map set)
       set))

;http://www.4clojure.com/problem/99 product digits
(def prod-digits
  (fn [one two]
    (map
      #(java.lang.Integer/parseInt (str %)) ; Can't pass Java static method e.g. java.lang.Integer/parseInt to map as a parameter!
      (seq (str (* one two))))))
(if false
  (prod-digits 1 1))
;others
(fn [a b] (map #(- (int %) 48) (str (* a b)))) ; apply map directly to a string
(fn [a b]
   (->> (* a b)
        str
        (map str)
        (map #(Integer/parseInt %))))
#(map (comp read-string str) (str (* % %2))) ;<<<
(fn [x y] (map #(Character/digit % 10) (str (* x y))))
(fn [a b] (map #(- (int %) (int \0)) (str (* a b))))









