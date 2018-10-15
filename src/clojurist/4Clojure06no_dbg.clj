(require 'clojure.set)
(require 'clojure.pprint)
;(clojure.main/load-script "/home/pkehl/GIT/clojurist/src/clojurist/dbg.clj")

;to reload this file in REPL, run:
;(clojure.main/load-script "/home/pkehl/GIT/clojurist/src/clojurist/4Clojure05.clj")

;http://www.4clojure.com/problem/101 Levenshtein Distance
;alternating wide and deep:
;-wide: generation of 1-step change alternatives
;-deep: iterating over the most promising alternatives first, based on a set ordered by number of differences 
(def leven
  (fn levensh [from target]
    (if (= from target) ;the below doesn't handle it. Too much work to refactor now.
       0
       (let [from (vec from) ;from string to a seq.
             target (vec target)
             num-of-generations (atom 0)
             item-type nil #_clojure.lang.Keyword #_java.lang.Long #_java.lang.Character] ;if item-type is nil, then don't validate items
         (letfn
           [(validate-items
              ([candidate msg]
               (validate-items candidate msg true))
              ([candidate msg do-fail] ;do-fail is to allow/supress assert, because a failed assert discards recent output
               (if item-type
                 (count
                   (for [i candidate]
                     (do
                       (if (not (instance? item-type i))
                         (do
                           (println "Candidate" msg candidate "has item" i "which is" (type i))
                           (flush)))
                       (if do-fail
                          (assert (instance? item-type i)
                                  (str "Candidate " candidate " when " msg " expected it to be " item-type " but it is " (type i) ": " i)))))))
               candidate))
            
            ;generate a seq of [candidate num-of-changes] with one-step changes ahead
            (next-candidates [[prev-candidate prev-num-of-changes]]
              (assert (not= prev-candidate target) "Reached already - should have been handled already")
              (validate-items prev-candidate "next-candidates->start")
              (assert (number? prev-num-of-changes) (str "Actual type" (type prev-num-of-changes) prev-num-of-changes))
              (let [prefix (vec (for [pair-of-items (map vector prev-candidate target) ;prefix is a shared initial part of: prev-candidate and target
                                      :while (= (first pair-of-items) (second pair-of-items))]
                                   (#_dbg #_:first_pair-of-chars first pair-of-items)))]
                #_TODO-for-end-of-prefix-onwards_change-each-index
                #_TODO-merge-two-let
                #_(dbg-println "Prev. candidate" prev-candidate "-> prefix" prefix)
                (println "Prev. candidate" prev-candidate "-> prefix" prefix)
                (let [prev-count (count prev-candidate)
                      target-count (count target)
                      prefix-count (count prefix)
                      next-num-of-changes (inc prev-num-of-changes)]
                  (validate-items prefix "prefix")
                  ; Here it used to add a character only if prev-candidate was of same length or shorter than target, and
                  ; remove a character only if prev-candidate was of same length or longer than target. However,
                  ; while that "heuristics" would accelerate most situations, it would prevent the best solutions
                  ; in "lookeahead" situations that require removal of character(s) even if the candidate is shorter, or
                  ; addition of character(s) if the candidate is longer, if more than a half of the rest of the candidate
                  ; matches target (and hence it overcompensates what first looks like a loss).
                  (#_dbg #_"str -> into [] with prefix" into
                    ; if a change reverts a previous change, we still count both changes. Such paths get eliminated by rating and by past-candidate-to-num.
                    (into () ;merge any possible & worthwhile candidates (1, 2 or 3) out of the following 3:
                      (if (< prefix-count target-count) ;otherwise we only need to remove the extra(s)
                        [[(validate-items (apply conj prefix
                                            (#_dbg #_"nth inc prefix-count1" nth target prefix-count) ;add 1 char; TODO revert from (nth) to (get) to trigger an error with string, where (println...) disappeared
                                            (drop      prefix-count  prev-candidate))
                            (str "add 1 char to " prefix " at target pos. " prefix-count " from target " target)) ;keep the rest
                          next-num-of-changes]
                         [(validate-items (apply conj prefix
                                            (#_dbg #_"nth inc prefix-count2" nth target prefix-count) ;replace 1 char
                                            (drop (inc prefix-count) prev-candidate))
                            "replace 1 char")
                          next-num-of-changes]] ;adjust the rest by 1 char
                        []))
                    (if (< prefix-count prev-count)
                      [[(validate-items (apply conj prefix
                                          (drop (inc prefix-count) prev-candidate))
                          "remove 1 char")
                        next-num-of-changes]] ;remove 1 char
                      [])))))
            
            ;toolkit on pairs/colls of [candidate num-of-changes]
            (compare-ignoring-effort [[cand1 _] [cand2 _]]
              (compare cand1 cand2))
            
            (diff [cand] ;number of differences in naive comparison char by char (as if we allowed char replacements only), plus a difference in length
              (apply +
                (int (Math/abs (- (count target) (count cand))))
                (map
                  #(if (= %1 %2) 1 0)
                  cand target)))
            
            (rate [[candidate num-changes]]
              (+ (diff candidate)
                 num-changes))
            
            (compare-full [[cand1 num-ch1 :as pair1] [cand2 num-ch2 :as pair2]]
              (validate-items cand1 "comp-full1") (validate-items cand2 "comp-full2")
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
              (identity ;to set the upper scope for dbgf of lazy seq.
                (for [pair (validate-queue previous-generation)
                      next-pair (next-candidates pair)]
                  next-pair)))
            ;----if slow, transform somehow into a lazy seq.
            
            (validate-queue [pairs]
              (count ;to consume the lazy sequence
                (for [[cand num] pairs]
                  (do
                    (validate-items cand "valid-queue")
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
           
           (loop [priority (sorted-set-by compare-full [from 0])
                  backlog (sorted-set-by compare-full)
                  best-num-changes nil
                  ;past-candidate-pairs (sorted-set-by compare-full) ;;set of [candidate num-changes] that were/are being already handled (i.e. in priority, backlog or thrown away)
                  past-candidate-to-num {from 0}] 
             ;best-num-changes is non-nil only once we have (any) results
             (assert (set? priority))
             (assert (set? backlog))
             (#_dbgf #_"validate priority" validate-queue priority)
             (#_dbgf #_"validate priority" validate-queue backlog)
             (if (and (= (count priority) 1) (empty? backlog) #_not-nil best-num-changes)
               (second (first priority)) ;this was supposed to be the (one) best result, but (at least for "kitten" -> "sitting" it wasn't reached!
               (if (and (seq backlog) (< (/ (count priority) (count backlog) 0.10 #_(tried 0.05, 0.30, 0.5)))) ;priority below a threshold, and backlog is non-empty => merge
                 (recur (into priority backlog) (empty backlog) best-num-changes past-candidate-to-num) ;<<<
                 (let [priority-moved-unfiltered (into (#_dbg #_"empty priority" empty priority)
                                                   (next-generation priority))
                       _ (validate-queue priority-moved-unfiltered)
                       
                       ;Collect two structures: a map and a set, both based on priority-moved-unfiltered and past-candidate-to-num.
                       ;map candidate => num-changes, based on a subset of priority-moved-unfiltered.
                       ;They were handled in the past, now found with the same or higher num-changes.
                       [priority-moved-past-same-or-worse-map
                        ;a subset of priority-moved-unfiltered. [cand num] that were handled in the past, now with more num-changes.
                        priority-moved-past-better]
                       (reduce
                         (fn [[worse better] [cand num :as new-pair]]
                           (validate-items cand "unfiltered->reduce")
                           (let [past-num (past-candidate-to-num cand)]
                             (if past-num
                               (if (<= past-num num)
                                 [(assoc worse cand num)
                                  better]
                                 [worse
                                  (conj better new-pair)])
                               [worse better])))
                         [{} (empty priority)]
                         priority-moved-unfiltered)
                       _ (validate-queue priority-moved-past-better)
                       
                       priority-moved-excluding-worse
                       (filter
                         (fn [[cand num]]
                           (validate-items cand "excl.worse")
                           (not (contains? priority-moved-past-same-or-worse-map cand)))
                         priority-moved-unfiltered)
                       _ (validate-queue priority-moved-excluding-worse)
                       
                       priority-moved
                       (apply conj
                         ;(re)inject any options that now look better. Being sets, conj keeps the items from the 1st set.
                         ;Hence the "better" set is the first param.
                         priority-moved-past-better
                         priority-moved-excluding-worse)
                       
                       past-candidate-to-num-next
                       (apply assoc
                         past-candidate-to-num
                         (for [ [cand num] priority-moved
                               key-or-value [cand num]]
                           (do
                             (validate-items cand "past-t-num")
                             (assert (number? num))
                             key-or-value)))
                       
                       _ (validate-queue priority-moved)
                       priority-moved-results (filter
                                                (fn [[cand _num]]
                                                  (validate-items cand "->prior.moved res.")
                                                  (assert (number? _num))
                                                  (= cand target))
                                                priority-moved)
                       _ (validate-queue priority-moved-results)
                       
                       priority-moved-best-num-changes
                       (if (seq priority-moved-results)
                         (apply min
                           (map
                             (fn [[_cand num-changes]]
                               (validate-items _cand "->best num ch.")
                               (assert (number? num-changes))
                               num-changes)
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
                                                (filter (fn [[_cand num-changes]]
                                                          (validate-items _cand "->to keep")
                                                          (< num-changes best-num))
                                                  candidates))
                                              candidates))
                       priority-keep   (candidates-to-keep priority-moved-unreached)
                       _ (validate-queue priority-keep)
                       backlog-keep (candidates-to-keep backlog)
                       _ (validate-queue backlog-keep)]
                   #_TODO-merge-previous-and-following-let
                   (let [priority-next-count (Math/ceil #_int (* (count priority-keep) 0.02 #_(tried 0.02, 0.03, 0.10, 0.30 with num-of-gen. limit 100)))
                         priority-next (into (empty priority) (take priority-next-count priority-keep))
                         _ (validate-queue priority-next)
                         backlog-next  (into backlog-keep (drop priority-next-count priority-keep))
                         _ (validate-queue backlog-next)]
                     (if (and (empty? priority-keep) (empty? backlog-keep))
                       best-num
                       (if (< @num-of-generations 350 #__400-too-much) ;limit number of generations - worthwhile for debugging
                         (do
                           (swap! num-of-generations inc)
                           (recur priority-next backlog-next best-num past-candidate-to-num-next))
                         :over-limit))))))))))))
(if false
  (leven "kitten" "sitting"))
;get for ordered set/map returns an *existing* entry, not the given key:
(assert (= ((sorted-set-by #(compare (mod % 3) (mod %2 3)) 1 2 4 5) 4) 1))
;conj for an existing key keeps the previous key and doesn't replace it.
;side note: maps/sets are compared regardless of any sorting
(= (conj (sorted-set-by #(compare (mod % 3) (mod %2 3)) 1 2) 4) #{1 2})
    
(seq? '[])
;(seqable? [])
    
    
    






























