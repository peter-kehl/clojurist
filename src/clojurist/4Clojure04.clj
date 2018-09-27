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







































