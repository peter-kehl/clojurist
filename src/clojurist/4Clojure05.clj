(require 'clojure.set)
(require 'clojure.pprint)

;to reload this file in REPL, run:
;(clojure.main/load-script "/home/pkehl/GIT/clojurist/src/clojurist/4Clojure05.clj")

;http://www.4clojure.com/problem/100 least common multiple
(def lcm
  (fn [& nums]
    (loop [pairs (into
                   (sorted-set-by ;of [num factor]
                        (fn [[n1 f1] [n2 f2]]
                          (compare (* n1 f1) (* n2 f2))))
                   (map
                     #(vector % 1)
                     nums))]
      (let [[candidate-num candidate-factor] (first pairs)
            candidate (* candidate-num candidate-factor)]
        (if (every?
              #(zero? (mod candidate %)) ;brute force
              nums)
          candidate
          (let [pairs-iterated (conj
                                 (disj pairs [candidate-num candidate-factor])
                                 [candidate-num (inc candidate-factor)])]
            (recur pairs-iterated)))))))

(if false
  (lcm 5 3 7))
(assert (instance? clojure.lang.BigInt 2N))
(assert (= 2 2N))
;others
(fn [& x]
    (let [g #(if (zero? %2) % (recur %2 (mod % %2)))
          l #(/ (* % %2) (g % %2))]
      (reduce l x)))
; lcm = product of numbers/greatest common divider
(fn [& args]
  (letfn 
    [(gcd [a b]
      (if (zero? b) a
          (gcd b (mod a b))))]
   (/
      (reduce *   args)
      (reduce gcd args))))
(fn [x & xs]
  (first
    (drop-while
      (fn [z] (some #(pos? (mod z %)) xs))
      (iterate #(+ x %) x))))

; insert `trace "description"` before function calls, like
; (trace "+ on numbers" + 1 2)
; Unfortunately, that disables compile time validation of number of parameters, so if functions/calls change,
; to validate them remove `trace "message", compile, re-add trace "message" back if need be.
; Beware when tracing errors: (for) creates a lazy sequence, hence callbacks will be delayed
;(debug "adding" (+ 1 1))
(defmacro debug [msg body]
      `(let [val# ~body]
         (println "DEBUG (MSG): " ~msg)
         (println "DEBUG (RET): " val#)
         val#))

(defn debug [msg & expression-parts]
  `((print "Calling" name "with ")   
    (clojure.pprint/pprint args)
    (try
      (let [res (apply f args)]
        (print name "returning ")
        (clojure.pprint/pprint res)
        res)
      (catch Throwable e
        (print "Threw" e)
        (throw e)))))
    

;http://www.4clojure.com/problem/101 Levenshtein Distance
;alternating wide and deep:
;-wide: generation of 1-step change alternatives
;-deep: iterating over the most promising alternatives first, based on a set ordered by number of differences 
(def leven
  (fn levensh [from to]
    (letfn
      [(diff [cand] ;number of differences in naive comparison char by char (as if we allowed char replacements only)
         (+
            (apply + ;good that (+) returns 0
              (map
                #(if (= %1 %2) 1 0)
                cand to))
            (int (Math/abs (- (count to) (count cand))))))
       
       ;generate a set of candidates with one-step changes
       (next-candidates [[prev-candidate prev-num-of-changes]]
         (let [prefix (apply str (for [pair-of-chars (map vector prev-candidate to) ;prefix is a shared initial part of: prev and to
                                       :while (= (first pair-of-chars) (second pair-of-chars))]
                                   (trace "first pair-of-chars" first pair-of-chars)))]
           #_TODO-for-end-of-prefix-onwards_change-each-index
           #_TODO-merge-two-let
           (println "Prefix " prefix)
           (let [prev-count (count prev-candidate)
                 to-count (count to)
                 prefix-count (count prefix)]
             (trace (str "str -> into prefix" prefix) into
               ; if a change reverts a previous change, we still count both changes. Such paths get eliminated by rating.
               (into [] ;merge 1 or 2 out of the following 3 candidate possibilities
                 (if (<= prev-count to-count)
                   [[(str prefix
                       (trace "get inc prefix-count1" get to (inc prefix-count)) ;added 1 char
                       (apply str (drop      prefix-count  prev-candidate)))
                     (inc prev-num-of-changes)] ;keep the rest
                    [(str prefix
                       (trace "get inc prefix-count2" get to (inc prefix-count)) ;replaced 1 char
                       (apply str (drop (inc prefix-count) prev-candidate)))]
                    (inc prev-num-of-changes)] ;adjust the rest by 1 char
                   []))
               (if (>= prev-count to-count)
                 [[(str prefix
                     (trace "into prefix 3: apply str" apply str (drop (inc prefix-count) prev-candidate)))]
                  (inc prev-num-of-changes)] ;removed
                 [])))))
       
       ;toolkit on pairs/colls of [candidate num-of-changes]
       (compare-ignoring-effort [[cand1 _] [cand2 _]]
         (compare cand1 cand2))
       
       (into-best-candidates [one two]
         (let [one-comparable (apply
                                sorted-set-by
                                compare-ignoring-effort
                                one)]
           (trace "into-best-candidates-> into" into
             (into #{}
               (for [from-two two]
                 (let [from-one (one-comparable from-two)]
                   (if (or (nil? from-one) (> (second from-one) (second from-two))) ;if two  candidates, take that with fewer changes
                     from-two
                     from-one))))
             one))) ;add any missing items from one, but don't replace existing items (which were already compared between both one and two) 
       
       (rate [[candidate num-changes]]
         (trace "+" + (diff candidate)
           num-changes))
       
       (compare-full [one two]
         (trace "compare-full"
           (fn [[cand1 num-ch1 :as pair1]
                [cand2 num-ch2 :as pair2]]
             (let [likeness-and-effort (comp (rate pair1) (rate pair2))]
               (if (zero? likeness-and-effort)
                 (comp cand1 cand2) ;this ensures we keep all candidates, including ones with same ranking, because their future may vary
                 likeness-and-effort)))
           one two))
       
       (next-generation [prev-gen] ; Parameter and result are of type: coll of [candidate num-of-changes]
         (for [p prev-gen
               n (trace "next-candidates" next-candidates p)
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
            (let [priority-moved (trace "into->priority-moved" into (trace "empty priority" empty priority)
                                   (trace "next-generation priority" next-generation priority))
                  priority-moved-results (filter (fn [cand _] (= cand to)) priority-moved)
                  ;priority-moved-results-nums (map (fn [_ num-changes] num-changes))
                  priority-moved-best-num-changes (if (seq priority-moved-results)
                                                    (apply min (map (fn [_ num-changes] num-changes)))
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
(if true
  (leven "kitten" "sitting"))
;get for ordered set/map returns an *existing* entry, not the given key:
(assert (= ((sorted-set-by #(compare (mod % 3) (mod %2 3)) 1 2 4 5) 4) 1))
;conj for an existing key keeps the previous key and doesn't replace it.
;side note: maps/sets are compared regardless of any sorting
(= (conj (sorted-set-by #(compare (mod % 3) (mod %2 3)) 1 2) 4) #{1 2})
    
(seq? '[])
(seqable? [])
    
    
    






























