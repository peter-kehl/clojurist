(require 'clojure.set)
(require 'clojure.pprint)

;to reload this file in REPL, run:
;(clojure.main/load-script "/home/pkehl/GIT/clojurist/src/clojurist/4Clojure05.clj")

;replacement for skipping the dbg, but only for forms with a string message: 
;(defmacro dbg [& args] (rest &form))

;macroexpand wants a quoted expression:
;(macroexpand '(dbg ...))

;http://www.4clojure.com/problem/101 Levenshtein Distance
;alternating wide and deep:
;-wide: generation of 1-step change alternatives
;-deep: iterating over the most promising alternatives first, based on a set ordered by number of differences 
(def leven
  (fn levensh [from to]
    (letfn
      [(diff [cand] ;number of differences in naive comparison char by char (as if we allowed char replacements only)
         (dbg "+" +
           (apply + ;good that (+) returns 0
             (map
               #(if (= %1 %2) 1 0)
               cand to))
           (int (Math/abs (- (count to) (count cand))))))
       
       ;generate a seq of [candidate num-of-changes] with one-step changes ahead
       (next-candidates [[prev-candidate prev-num-of-changes]]
         (assert (number? prev-num-of-changes))
         (let [prefix (dbg "apply str" apply str (for [pair-of-chars (map vector prev-candidate to) ;prefix is a shared initial part of: prev and to
                                                       :while (= (first pair-of-chars) (second pair-of-chars))]
                                                   (dbg "first pair-of-chars" first pair-of-chars)))]
           #_TODO-for-end-of-prefix-onwards_change-each-index
           #_TODO-merge-two-let
           (dbg-println "Prefix " prefix)
           (let [prev-count (count prev-candidate)
                 to-count (count to)
                 prefix-count (count prefix)]
             (dbg "str -> into [] with prefix" into
               ; if a change reverts a previous change, we still count both changes. Such paths get eliminated by rating.
               (dbg "into" into '() ;merge 1 or 2 out of the following 3 candidate possibilities
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
       
       ; merge candidate pairs from one and two, ignoring the effort. Used when merging backlog to priority, so that every possibility gets a chance.
       ;TODO why isn't this called at all?
       (into-best-candidates [one two]
         (let [one-comparable (apply
                                sorted-set-by
                                compare-ignoring-effort
                                one)]
           (dbg "into-best-candidates-> into" into
             (into #{}
               (for [from-two two]
                 (let [from-one (one-comparable from-two)]
                   (if (or
                           (nil? from-one)
                           (> (second from-one) (second from-two))) ;if two  candidates, take that with fewer changes
                     from-two
                     from-one))))
             one))) ;add any missing items from one, but don't replace existing items (which were already compared between both one and two) 
       
       (rate [[candidate num-changes]]
         (dbg "+" + (diff candidate)
           num-changes))
       
       (compare-full [one two]
         (dbg "compare-full"
           (fn [[cand1 num-ch1 :as pair1]
                [cand2 num-ch2 :as pair2]]
             (let [likeness-and-effort (comp (rate pair1) (rate pair2))]
               (if (zero? likeness-and-effort)
                 (comp cand1 cand2) ;this ensures we keep all candidates, including ones with same ranking, because their future may vary
                 likeness-and-effort)))
           one two))
       
       (next-generation [prev-gen] ; Parameter and result are of type: coll of [candidate num-of-changes]
         (for [p prev-gen
               n (dbg "next-candidates" next-candidates p)
               c n]
           c))]
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
      
      (loop [priority (sorted-set-by compare-full [[from 0]])
             backlog (sorted-set-by compare-full)
             best-num-changes nil] ;best-num-changes is non-nil only once we have (any) results
        
        (if (and (= (count priority) 1) (empty? backlog) #_not-nil best-num-changes)
          (second (first priority))
          (if (and (seq backlog) (< (/ (count priority) (count backlog) 0.05)))
            (recur (into priority backlog) (empty backlog) best-num-changes)
            (let [priority-moved (dbg "into->priority-moved" into (trace "empty priority" empty priority)
                                   (dbg "next-generation priority" next-generation priority))
                  priority-moved-results (filter (fn [cand _] (= cand to)) priority-moved)
                  ;priority-moved-results-nums (map (fn [_ num-changes] num-changes))
                  priority-moved-best-num-changes (if (seq priority-moved-results)
                                                    (apply min (map (fn [_ num-changes] num-changes) priority-moved-results))
                                                    nil)
                  best-num (if best-num-changes
                             (if priority-moved-best-num-changes
                               (min best-num-changes priority-moved-best-num-changes)
                               best-num-changes)
                             priority-moved-best-num-changes)
                  priority-moved-unreached (disj priority-moved priority-moved-results)
                  candidates-to-keep (fn [candidates]
                                       (if best-num
                                         (into (empty priority)
                                           (filter (fn [_ num-changes] (< num-changes best-num))
                                             candidates))
                                         candidates))
                  priority-keep   (candidates-to-keep priority-moved-unreached)
                  backlog-keep (candidates-to-keep backlog)]
              #_TODO-merge-previous-and-following-let
              (let [priority-next-count (int (* (count priority-keep) 0.10))
                    priority-next (take priority-next-count priority-keep)
                    backlog-next  (into backlog-keep (drop priority-next-count priority-keep))]
                
                (recur priority-next backlog-next best-num)))))))))
(if false
  (leven "kitten" "sitting"))
;get for ordered set/map returns an *existing* entry, not the given key:
(assert (= ((sorted-set-by #(compare (mod % 3) (mod %2 3)) 1 2 4 5) 4) 1))
;conj for an existing key keeps the previous key and doesn't replace it.
;side note: maps/sets are compared regardless of any sorting
(= (conj (sorted-set-by #(compare (mod % 3) (mod %2 3)) 1 2) 4) #{1 2})
    
(seq? '[])
(seqable? [])
    
    
    





























