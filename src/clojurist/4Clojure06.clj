(require 'clojure.set)
(require 'clojure.pprint)
(clojure.main/load-script "/home/pkehl/GIT/clojurist/src/clojurist/dbg.clj")

;to reload this file in REPL, run:
;(clojure.main/load-script "/home/pkehl/GIT/clojurist/src/clojurist/4Clojure05.clj")

;http://www.4clojure.com/problem/101 Levenshtein Distance
;alternating wide and deep:
;-wide: generation of 1-step change alternatives
;-deep: iterating over the most promising alternatives first, based on a set ordered by number of differences 
(def leven
  (fn levensh [from to]
    (letfn
      [(diff [cand] ;number of differences in naive comparison char by char (as if we allowed char replacements only)
         (#_dbg #_"+" +
           (apply + ;good that (+) returns 0
             (map
               #(if (= %1 %2) 1 0)
               cand to))
           (int (Math/abs (- (count to) (count cand))))))
       
       ;generate a seq of [candidate num-of-changes] with one-step changes ahead
       (next-candidates [[prev-candidate prev-num-of-changes]]
         (println "prev-num-of-changes" (type prev-num-of-changes) "and prev-candidate" prev-candidate)
         (assert (number? prev-num-of-changes) (str "Actual type" (type prev-num-of-changes) prev-num-of-changes))
         (let [prefix (dbg :apply-str apply str (for [pair-of-chars (map vector prev-candidate to) ;prefix is a shared initial part of: prev and to
                                                      :while (= (first pair-of-chars) (second pair-of-chars))]
                                                  (dbg :first_pair-of-chars first pair-of-chars)))]
           #_TODO-for-end-of-prefix-onwards_change-each-index
           #_TODO-merge-two-let
           ;(dbg-println "Prefix " prefix)
           (let [prev-count (count prev-candidate)
                 to-count (count to)
                 prefix-count (count prefix)]
             (dbg "str -> into [] with prefix" into
               ; if a change reverts a previous change, we still count both changes. Such paths get eliminated by rating.
               (dbg "into ()" into () ;merge 1 or 2 out of the following 3 candidate possibilities
                 (if (<= prev-count to-count)
                   [[(str prefix
                       (dbg "get inc prefix-count1" get to (inc prefix-count)) ;added 1 char
                       (apply str (drop      prefix-count  prev-candidate)))
                     (inc prev-num-of-changes)] ;keep the rest
                    [(str prefix
                       (dbg "get inc prefix-count2" get to (inc prefix-count)) ;replaced 1 char
                       (apply str (drop (inc prefix-count) prev-candidate)))
                     (inc prev-num-of-changes)]] ;adjust the rest by 1 char
                   []))
               (if (>= prev-count to-count)
                 [[(str prefix
                     (dbg "into prefix 3: apply str" apply str (drop (inc prefix-count) prev-candidate)))
                   (inc prev-num-of-changes)]] ;removed
                 [])))))
       
       ;toolkit on pairs/colls of [candidate num-of-changes]
       (compare-ignoring-effort [[cand1 _] [cand2 _]]
         (compare cand1 cand2))
       
       (rate [[candidate num-changes]]
         (#_dbg #_"+" + (diff candidate)
            num-changes))
       
       (compare-full [one two]
         (#_dbg #_"compare-full"
          (fn [[cand1 num-ch1 :as pair1]
               [cand2 num-ch2 :as pair2]]
            (let [likeness-and-effort (comp (rate pair1) (rate pair2))]
              (if (zero? likeness-and-effort)
                (comp cand1 cand2) ;this ensures we keep all candidates, including ones with same ranking, because their future may vary
                likeness-and-effort)))
          one two))
       
       (next-generation [previous-generation] ; Parameter and result are of type: coll of [candidate num-of-changes]
         (dbg :count count ;count forces the lazy
           (for [pair (dbg :count :validate-queue validate-queue previous-generation)
                 n (dbg :count :next-candidates next-candidates pair)
                 c n]
               c)))
       (validate-queue [pairs]
         (print "validate-queue")
         (clojure.pprint/pprint pairs)
         (count ;to consume the lazy sequence
            (for [[cand num] pairs]
              (do
                (println "validating cand" cand "num" num)
                (assert (string? cand))
                (assert (number? num))
                (println "validating cand - after assert"))))
         (println "after for")
         pairs)]
       
      ;if slow, transform somehow into a lazy seq.
      
      
      ; heuristics: wide-deep combination: Give more chance to the most promising candidates.
      ; But occasionally progress others, too, because one step can move a candidate ahead (much closer to the result).
      ; Sort candidates.
      ; Split candidates (e.g. by a certain limit) into priority & backlog.
      ; Iterate priority. Split the result. The rest of candidates goes to the backlog.
      ; Repeat a few times.
      ; Iterate both priority & backlog, merge, split.
      ; Once you find one result, remove all candidates that would take same number of steps or more.
      ; Repeat until all candidates reach (and obviously have same number of steps).
      
      (dbgloop [priority (sorted-set-by compare-full [from 0])
                backlog (sorted-set-by compare-full)
                best-num-changes nil]
        ;best-num-changes is non-nil only once we have (any) results
        (validate-queue priority)
        (validate-queue backlog)
        (if (and (= (count priority) 1) (empty? backlog) #_not-nil best-num-changes)
          (second (first priority))
          (if (and (seq backlog) (< (/ (count priority) (count backlog) 0.05)))
            (recur (into priority backlog) (empty backlog) best-num-changes) ;<<<
            (let [priority-moved (#_dbg #_"into->priority-moved" into (#_dbg #_"empty priority" empty priority)
                                   (dbg next-generation priority)) ;<<<
                  _ (validate-queue priority-moved)
                  priority-moved-results (filter (fn [cand _] (= cand to)) priority-moved)
                  _ (validate-queue priority-moved-results)
                  ;priority-moved-results-nums (map (fn [_ num-changes] num-changes))
                  priority-moved-best-num-changes (if (seq priority-moved-results)
                                                    (apply min (map (fn [_ num-changes] num-changes) priority-moved-results))
                                                    nil)
                  _ (validate-queue priority-moved-best-num-changes)
                  best-num (if best-num-changes
                             (if priority-moved-best-num-changes
                               (min best-num-changes priority-moved-best-num-changes)
                               best-num-changes)
                             priority-moved-best-num-changes)
                  priority-moved-unreached (disj priority-moved priority-moved-results)
                  _ (validate-queue priority-moved-unreached)
                  candidates-to-keep (fn [candidates]
                                       (if best-num
                                         (into (empty priority)
                                           (filter (fn [_ num-changes] (< num-changes best-num))
                                             candidates))
                                         candidates))
                  _ (validate-queue candidates-to-keep)
                  priority-keep   (candidates-to-keep priority-moved-unreached)
                  _ (validate-queue priority-keep)
                  backlog-keep (candidates-to-keep backlog)
                  _ (validate-queue priority-keep)]
              #_TODO-merge-previous-and-following-let
              (let [priority-next-count (int (* (count priority-keep) 0.10))
                    priority-next (take priority-next-count priority-keep)
                    _ (validate-queue priority-next)
                    backlog-next  (into backlog-keep (drop priority-next-count priority-keep))
                    _ (validate-queue backlog-next)]
                
                (dbgrecur priority-next backlog-next best-num))))))))) ;<<<
(if false
  (leven "kitten" "sitting"))
;get for ordered set/map returns an *existing* entry, not the given key:
(assert (= ((sorted-set-by #(compare (mod % 3) (mod %2 3)) 1 2 4 5) 4) 1))
;conj for an existing key keeps the previous key and doesn't replace it.
;side note: maps/sets are compared regardless of any sorting
(= (conj (sorted-set-by #(compare (mod % 3) (mod %2 3)) 1 2) 4) #{1 2})
    
(seq? '[])
(seqable? [])
    
    
    






























