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
    (if (= from to) ;the below doesn't handle it. Too much work to refactor now.
       0
       (let [num-of-generations (atom 0)
             item-type nil #_clojure.lang.Keyword #_java.lang.Long #_java.lang.Character] ;if item-type is nil, then don't validate items
         (letfn
           [;generate a seq of [candidate num-of-changes] with one-step changes ahead
            (next-candidates [[prev-candidate prev-num-of-changes]]
              (if item-type
                  (count
                      (for [i prev-candidate]
                          (assert (instance? item-type i)))))
              (assert (number? prev-num-of-changes) (str "Actual type" (type prev-num-of-changes) prev-num-of-changes))
              (let [prefix (vec (for [pair-of-items (map vector prev-candidate to) ;prefix is a shared initial part of: prev and to
                                      :while (= (first pair-of-items) (second pair-of-items))]
                                   (#_dbg #_:first_pair-of-chars first pair-of-items)))]
                #_TODO-for-end-of-prefix-onwards_change-each-index
                #_TODO-merge-two-let
                (dbg-println "Prefix " prefix)
                (let [prev-count (count prev-candidate)
                      to-count (count to)
                      prefix-count (count prefix)]
                  (#_dbg #_"str -> into [] with prefix" into
                    ; if a change reverts a previous change, we still count both changes. Such paths get eliminated by rating.
                    (into () ;merge 1 or 2 out of the following 3 candidate possibilities
                      (if (<= prev-count to-count)
                        [[(apply conj prefix
                            (#_dbg #_"get inc prefix-count1" get to prefix-count) ;add 1 char
                            (drop      prefix-count  prev-candidate))
                          (inc prev-num-of-changes)] ;keep the rest
                         [(apply conj prefix
                            (#_dbg #_"get inc prefix-count2" get to prefix-count) ;replace 1 char
                            (drop (inc prefix-count) prev-candidate))
                          (inc prev-num-of-changes)]] ;adjust the rest by 1 char
                        []))
                    (if (>= prev-count to-count)
                      [[(apply conj prefix
                          (drop (inc prefix-count) prev-candidate))
                        (inc prev-num-of-changes)]] ;remove 1 char
                      [])))))
            
            ;toolkit on pairs/colls of [candidate num-of-changes]
            (compare-ignoring-effort [[cand1 _] [cand2 _]]
              (compare cand1 cand2))
            
            (diff [cand] ;number of differences in naive comparison char by char (as if we allowed char replacements only), plus a difference in length
              (apply +
                (int (Math/abs (- (count to) (count cand))))
                (map
                  #(if (= %1 %2) 1 0)
                  cand to)))
            
            (rate [[candidate num-changes]]
              (+ (diff candidate)
                 num-changes))
            
            (compare-full [[cand1 num-ch1 :as pair1] [cand2 num-ch2 :as pair2]]
              (let [likeness-and-effort (compare (rate pair1) (rate pair2))]
                (if (zero? likeness-and-effort)
                  (compare cand1 cand2) ;this ensures we keep all candidates, including ones with same ranking, because their future may vary
                  likeness-and-effort)))
            ;TODO to dbg doc: 
            ; 1. (dbgs :user-provided) ensures that, after debugging that line, once you remove dbgf or dbg, you won't leave this scope indicator forgotten 
            ;---- During a call to (dbgf ..) or (dbg ..) at a higher level, that macro generates (let []) with its "scope consuming" symbol.
            ; --- At this (lower) level, (dbgs ..) macro uses &env to detect whether that scope was "consumed" already. If so, fail (at macro level) -> compile time.
            ; ------but: (dbgs ...) generates a compile time list. Hence (dbgf ...) and (dbg ...) would have to inspect it, so to determine if it's a user-provided function-generating form.
            ; ------- or: dbgf and dbg accept a 2nd param, *required*, a symbol, but then it'd need to check at *runtime*!
            ; ------- or: 2nd param only a string literal, not a keyword literal. If the user leaves it behind, it fails (can't be cast ot IFn). 
            ; 2. scope for lazy seq ->vvv
            (next-generation [previous-generation] ; Parameter and result are of type: coll of [candidate num-of-changes]
              (dbgf :next-generation identity ;to set the upper scope for dbgf of lazy seq.
                (for [pair (dbgf :next-generation :next-gen->validate-queue validate-queue previous-generation)
                      next-pair (dbgf :next-generation :next-gen->next-candidates next-candidates pair)]
                  next-pair)))
            ;----if slow, transform somehow into a lazy seq.
            
            (validate-queue [pairs]
              (count ;to consume the lazy sequence
                (for [[cand num] pairs]
                  (do
                    (if item-type
                       (count
                         (for [i cand]
                             (assert (instance? item-type i)))))
                    (assert (number? num)))))
              pairs)]
           
           ; heuristics: wide-deep combination: Give more chance to the most promising candidates.
           ; But occasionally progress others, too, because one step can move a candidate ahead (much closer to the result).
           ; Sort candidates.
           ; Split candidates (e.g. by a certain limit) into priority & backlog.
           ; Iterate priority. Split the result. The rest of candidates goes to the backlog.
           ; Repeat a few times.
           ; Iterate both priority & backlog, merge, split.
           ; Once you find one result, remove all candidates that would take same number of steps or more.
           ; Repeat until all candidates reach (and obviously have same number of steps).
           
           (dbgloop [priority (dbgf sorted-set-by compare-full [from 0])
                     backlog (sorted-set-by compare-full)
                     best-num-changes nil]
             ;best-num-changes is non-nil only once we have (any) results
             (assert (set? priority))
             (assert (set? backlog))
             (#_dbgf #_"validate priority" validate-queue priority)
             (#_dbgf #_"validate priority" validate-queue backlog)
             (if (and (= (count priority) 1) (empty? backlog) #_not-nil best-num-changes)
               (second (first priority)) ;this was supposed to be the (one) best result, but (at least for "kitten" -> "sitting" it wasn't reached!
               (if (and (seq backlog) (< (/ (count priority) (count backlog) 0.05))) ;priority below a threshold, and backlog is non-empty => merge
                 (dbgrecur (into priority backlog) (empty backlog) best-num-changes) ;<<<
                 (let [priority-moved (into (#_dbg #_"empty priority" empty priority)
                                        (dbgf :next-generation next-generation priority))
                       _ (validate-queue priority-moved)
                       priority-moved-results (filter
                                                (fn [[cand _]] (= cand to))
                                                priority-moved)
                       _ (validate-queue priority-moved-results)
                       ;_ (dbg-pprint-last "priority-moved-results" priority-moved-results) _ (println)
                       priority-moved-best-num-changes
                       (if (seq priority-moved-results)
                         (apply min
                           (map
                             (fn [[_ num-changes]] num-changes)
                             priority-moved-results))
                         nil)
                       best-num (if best-num-changes
                                  (if priority-moved-best-num-changes
                                    (min best-num-changes priority-moved-best-num-changes)
                                    best-num-changes)
                                  priority-moved-best-num-changes)
                       
                       ;_ (dbg-pprint-last "priority-moved" priority-moved) _ (println)
                       
                       priority-moved-unreached (apply disj priority-moved priority-moved-results)
                       _ (validate-queue priority-moved-unreached)
                       candidates-to-keep (fn [candidates]
                                            (if best-num
                                              (into (empty priority)
                                                (filter (fn [[_ num-changes]] (< num-changes best-num))
                                                  candidates))
                                              candidates))
                       priority-keep   (candidates-to-keep priority-moved-unreached)
                       _ (validate-queue priority-keep)
                       backlog-keep (candidates-to-keep backlog)
                       _ (validate-queue backlog-keep)]
                   #_TODO-merge-previous-and-following-let
                   (let [priority-next-count (int (* (count priority-keep) 0.10))
                         priority-next (into (empty priority) (take priority-next-count priority-keep))
                         _ (validate-queue priority-next)
                         backlog-next  (into backlog-keep (drop priority-next-count priority-keep))
                         _ (validate-queue backlog-next)]
                     (if (and (empty? priority-keep) (empty? backlog-keep))
                       (str "Emptied " best-num)
                       (if (< @num-of-generations 100) ;limit number of generations - worthwhile for debugging
                         (do
                           (swap! num-of-generations inc)
                           (dbgrecur priority-next backlog-next best-num))
                         :over-limit))))))))))))
(if false
  (leven "kitten" "sitting"))
;get for ordered set/map returns an *existing* entry, not the given key:
(assert (= ((sorted-set-by #(compare (mod % 3) (mod %2 3)) 1 2 4 5) 4) 1))
;conj for an existing key keeps the previous key and doesn't replace it.
;side note: maps/sets are compared regardless of any sorting
(= (conj (sorted-set-by #(compare (mod % 3) (mod %2 3)) 1 2) 4) #{1 2})
    
(seq? '[])
(seqable? [])
    
    
    






























