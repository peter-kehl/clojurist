(require 'clojure.set)
;(clojure.main/load-script "/home/pkehl/GIT/clojurist/src/clojurist/dbg.clj")

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
(if false
  (transit #{[8 4] [9 3] [4 2] [27 9]}))

(if false
  (let [divides #{[8 4] [9 3] [4 2] [27 9]}]
    (= (transit divides) #{[4 2] [8 4] [8 2] [9 3] [27 9] [27 3]})))

;http://www.4clojure.com/problem/125
; no macros e.g. #() - because they expand from (str 'code)!
; ok to indent - because (str 'code) ignores extra whitespace
(def gus (fn []))
(gus)

;(defmacro gusm[] '(fn []))

( (fn [pref post]
    (fn []))
 "a" "b")


(let [glob (fn [pref post] (let [x 1]))])

(defmacro gusm[]
  '(partial str [\( \)])
  #_end)
(gusm)
;(println ((gusm)))

; eval is not allowed
(let [global '(fn [itself]
                (str "(let [global '" itself "] ((eval global) global)"))]
  ((eval global) global)
  #_end)

(eval '(eval 1))

;EvalReader not allowed - *read-eval* is false
(read-string "#=(identity 1)")
;(identity "identity")

;(char-escape-string \' ) ;only for string literal-special chars, not for e.g. () {}
;clojure.string/escape

;(clojure.string/replace "\\-\"c" #"([\"\\])" "\\\\$1")

; it has to generate a function
; it has to include outer ()
; limit special characters

;copied from the below prefixed with an apostrophe ' as '(let...):
(let [esc (fn [text] (clojure.string/replace text "\"" "\\\"")) twice (fn [pre post] (list pre (esc pre) " " (esc post) post))] (fn [] (apply str (twice "((let [esc (fn [text] (clojure.string/replace text \"\\\"\" \"\\\\\\\"\")) twice (fn [pre post] (list pre (esc pre) \" \" (esc post) post))] (fn [] (apply str (twice" ")))))")))

; to generate code for 4clojure.com, pust an apostrophe in front of (let ...), and copy the text of the generated tree.
;(println
 '(let [  esc (fn [text]
                (clojure.string/replace text #"[\"\\]" "\\\\$0"))
        twice (fn [pre post]
                (list pre \" (esc pre) "\" \"" (esc post) \" post))]
    (fn []
      (apply str (twice "(let [esc (fn [text] (clojure.string/replace text #\"[\\\"\\\\]\" \"\\\\\\\\$0\")) twice (fn [pre post] (list pre \\\" (esc pre) \"\\\" \\\"\" (esc post) \\\" post))] (fn [] (apply str (twice " "))))")))))

;the exact (unindented) solution:
(let [esc (fn [text] (clojure.string/replace text #"[\"\\]" "\\\\$0")) twice (fn [pre post] (list pre \" (esc pre) "\" \"" (esc post) \" post))] (fn [] (apply str (twice "(let [esc (fn [text] (clojure.string/replace text #\"[\\\"\\\\]\" \"\\\\\\\\$0\")) twice (fn [pre post] (list pre \\\" (esc pre) \"\\\" \\\"\" (esc post) \\\" post))] (fn [] (apply str (twice " "))))"))))
;------


;http://www.4clojure.com/problem/128 
;Suit: Spades, Hearts, Diamonds, and Clubs 
;Rank: 2..9, 10 ("T"), Jack, Queen, King, and Ace -> here 0..12
(def code2struc
  (fn [[suit rank]]
    {:suit ({\S :spade \H :heart \D :diamond \C :club} suit)
     :rank (let [ascii (int rank)]
             (if (<= ascii (int \9))
               (- ascii (int \2))
               ({\T 8 \J 9 \Q 10 \K 11 \A 12} rank)))}))

(code2struc "CA")

;http://www.4clojure.com/problem/178 best hand poker
(def best-hand
  (fn [codes]
    (let [code2struc (fn [[suit rank]]
                       {:suit ({\S :spade \H :heart \D :diamond \C :club} suit)
                        :rank (let [ascii (int rank)]
                                (if (<= ascii (int \9))
                                  (- ascii (int \2))
                                  ({\T 8 \J 9 \Q 10 \K 11 \A 12} rank)))})
          strucs (map code2struc codes)
          
          suits (map :suit strucs)
          ranks (map :rank strucs)
          
          same-rank (apply = suits)
          standard-sequence (= (- (max ranks) (min ranks))
                               4)
          by-rank-inv (into
                        (sorted-map-by
                          (comp - count)) ;most frequent rank first; can't use (reverse ...) as that turns a map into a sequence - bad for (vals ...) below
                        (clojure.set/map-invert
                          (group-by :rank strucs)))]
      (cond
        (and same-rank standard-sequence)
        :straight-flush
        
        (some (fn [[_ cards]]
                (= (count cards) 4))
          by-rank)
        :four-of-a-kind
      
        (and (= (count by-rank-inv) 2)
             (= (count (first (sort-by count (vals by-rank) 2))))) ;the other group must have 3 cards, since we have 2 groups only
        :full-house
        
        same-rank
        :flush
        
        (or standard-sequence
            (let [non-aces (filter #(not= 12 %) ranks)]
              (and (= (count non-aces) 4)
                   (= (- (max non-aces) (min non-aces))
                      3))))
        :straight
        
        
        :three-of-a-kind
        
        :two-pair
        
        :pair
        
        :else
        :high-card))))
      


;http://www.4clojure.com/problem/130 tree reparent
(def reparent
  (fn [orig]))
    
    

;http://www.4clojure.com/problem/111 crossword
(def cross
  (fn [goal mx]
    ; row and col are 0-based; col is lower than num-cols, i.e. it "skips" whitespaces (assuming well/consistently-positioned whitespace)
    (let [goal (seq goal)
          places (fn [mixed] (filter #(not= \space %) mixed)) ;mixed is a row, or a column. See also coll-full.
          first-row (mx 0)
          num-cols (count (places first-row))
          
          col-full (fn [col]
                     ; An outer (places ...) is not needed.
                     (map #(nth
                             (places %)
                             col)
                       mx))
          row-full (fn [row]
                     (places (mx row))) ;assuming "good" formatting
          ;slice of a column, starting at [row col] position
          col (fn [r-index c-index] (drop r-index (col-full c-index)))
          row (fn [r-index c-index] (drop c-index (row-full r-index)))
          
          count-goal (count goal)
          ;whether goal fits from the start of slice, up to end of slice or up to \#
          fits (fn [slice]
                 (and (or
                          (= count-goal (count slice))
                          (and (< count-goal (count slice))
                               (= \# (nth slice count-goal))))
                      (every? identity ;can't (apply and ...) because and is a macro
                        (map #(#{% \_} %2) goal slice))))
          place (fn [r-index c-index] ;it expects "good" formatting
                  (nth (row-full r-index) c-index))]
      
      (boolean
        (some identity
          (for [r-index (range 0 (count mx))
                c-index (range 0 num-cols)]
            (or (and (or (zero? c-index)
                         (= \# (place r-index (dec c-index))))
                     (fits (row r-index c-index)))
                (and (or (zero? r-index)
                         (= \# (place (dec r-index) c-index)))
                     (fits (col r-index c-index))))))))))
(if false
 (cross "joy" ["c _ _ _"
               "d _ # e"
               "r y _ _"]))
(if false
  (cross "the" ["c _ _ _"
                "d _ # e"
                "r y _ _"]))

;http://www.4clojure.com/problem/195 Parenthesis combinations
; (((( ))))
; n =>> n+1: ( around ), () before, with () in - at any index, after ()
; NOT handling: with in n - but ( around a subset ) of n - at any possible index
; - if the result is whole within (), then such a result can be accomplished by the above rule: ( around )
; - otherwise the result is within 2+ () groups. Each was generated at lower level. It should have been
;   embraced in another top level () pair as per above rules.
(def parens
  (fn [target-n]
    (loop [prev #{""}
           prev-n 0]
      (if (= target-n prev-n)
        prev
        (let [n (inc prev-n)
              expand (fn [grp]
                       (assert (= (* 2 prev-n) (count grp)))
                       (into ;around the same speed as: apply conj (str...) (str...) (for....)
                         (list (#_dbgf #_"str1" str \( grp \))
                           (#_dbgf #_"str2" str grp "()"))
                         (for [i (range 0 (count grp))]
                           (#_dbgf #_"for-> str" str (subs grp 0 i) "()" (subs grp i)))))]
          
          (recur
            (into #{} ;about the same speed as apply conj #{}...
              (apply concat ;% faster than: reduce into #{}
                (#_dbgf #_"map" map expand prev)))
            n))))))

; Generate a set of seq first, then make them strings. That saves creation of string duplicates. But it was 3.5x slower than with strings!
; n=12 => 2.1 sec with strings; 7.4sec with vectors!
(def parens
  (fn [target-n]
    (loop [prev #{[]} ;always vectors -> easy (conj..)
           prev-n 0]
      (if (= target-n prev-n)
        prev
        (let [n (inc prev-n)
              expand (fn [grp]
                       (assert (= (* 2 prev-n) (count grp)))
                       (into ;around the same speed as: apply conj (str...) (str...) (for....)
                         (list
                           (conj (vec (cons \( grp)) \))
                           (conj grp \( \)))
                         (for [i (range 0 (count grp))]
                           (into (conj (subvec grp 0 i) \( \) ) (subvec grp i)))))]
          
          (recur
            (into #{} ;about the same speed as apply conj #{}...
              (apply concat ;% faster than: reduce into #{}
                (#_dbgf #_"map" map expand prev)))
            n))))))


;----
; start with ((((...)))), then move the rightmost \( step by step to the right (as far as possible)
;            (()...())()
(def parens
  (fn [n]
    (if (zero? n)
      #{}
      (let [end (dec (* 2 n))]
        
        (loop [prev (apply str (concat (repeat n \() (repeat n \))))
               res #{prev}]
          ;(dbg-print :prev prev)
          (let [rightmost-movable-opener (fn []
                                           (loop [pos end
                                                  num-right-closers 0]
                                             (if (zero? pos)
                                               nil
                                               (if (= (nth prev pos) \()
                                                 (if (> num-right-closers 1)
                                                   pos
                                                   (recur (dec pos) (dec num-right-closers)))
                                                 (recur   (dec pos) (inc num-right-closers))))))
                opener (rightmost-movable-opener)]
            (if opener
              ;  v<-- opener
              ; (())()
              ;  
              (let [_ (assert (= (nth prev (inc opener)) \)))
                    now (str
                          (subs prev 0 opener)
                          \) ; <-- right after opener must come a closer. Otherwise this opener wouldn't be the rightmost.
                          \(
                          (subs prev (+ opener 2)))]
                
                (recur now (conj res now)))
              res)))))))

; the above was not covering all: n=3 -> only  #{"(()())" "((()))" "()()()" "(())()"}
; missing (()()) -> ()(()) <-- when moving to the right the 2nd/3rd/farther... rightmost opener first
; -> then "jump over" the other rightmost opener(s)
; - treat a consecutive group of openers (( or ((( or ((((... as the same - it doesn't matter which of them you move to the right
; --- hence act only on 2nd/3rd/4th... openers ( that have a closer ) immediately right from them
(def parens
  (fn [n]
    (if (zero? n)
      #{}
      (let [end (dec (* 2 n))]
        
        (loop [prev-grp #{(apply str (concat (repeat n \() (repeat n \))))}
               res #{(first prev-grp)}]
          ;(dbg-print :prev prev)
          (let [rightmost-movable-openers (fn [prev]
                                            (loop [res () ;O the highest indexes to the right -> easy (< pos ...) below
                                                   pos end
                                                   num-right-closers 0]
                                              (if (zero? pos)
                                                res
                                                (if (= \( (nth prev pos))
                                                  (if (and
                                                           (> num-right-closers 1)
                                                           (= \) (nth prev (inc pos)))) ; having a closer ) immediately on the right
                                                    (recur (cons pos res) (dec pos) (dec num-right-closers))
                                                    (recur           res  (dec pos) (dec num-right-closers)))
                                                  (recur             res  (dec pos) (inc num-right-closers))))))
                
                now (#_dbg #_"now <- for" for [prev prev-grp
                                               opener (#_dbgf rightmost-movable-openers prev)]
                      (do
                        (assert (= \)  (nth prev (inc opener))))
                        ;  v<-- opener
                        ; (())()
                        ;  
                        (str
                          (subs prev 0 opener)
                          \) ; <-- right after opener must come a closer. Otherwise this opener wouldn't be the rightmost.
                          \(
                          (subs prev (+ opener 2)))))]
            (if (seq now)
              (recur now (into res now))
              res)))))))
                                                 
                       
;      let [opener (rightmost-movable-opener)])))

;(parens 0)
;(parens 1)
(parens 3)
#_(count (parens 10))
#_(time (= (nth (sort (parens 12)) 5000) "(((((()()()()()))))(()))")) ;2.1 sec with strings; 7.4sec with vectors!










































