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



;http://www.4clojure.com/problem/124 Reversi game available moves. https://en.wikipedia.org/wiki/Reversi:
; any disks of the opponent's color that are in a straight line and bounded by
;  the disk just placed and another disk of the current player's color are turned over to the current player's color. 
  ; - a valid move is one where at least one piece is reversed. 
(def reversi
  (fn [board my-colour]
    (let [colour-at (fn [coordinates]
                      {:pre (= (count coordinates) 2)
                       :post [(not= % nil)]};))}
                      (get-in board coordinates))
          move-to-pos (fn [[from-x from-y :as from] [change-x change-y]] ;return new coordinates
                        {:pre  [(colour-at from) (<= -1 change-x 1) (<= -1 change-y 1)]
                         :post [(colour-at %) (not= % from)]}
                        [(+ from-x change-x) (+ from-y change-y)])
          width 4
          max-pos (dec width)
          
          ; get all physically available directions & max distance (1..3) per available direction
          direction-radius (fn [[from-x from-y :as from]] ; return {[change-x change-y] max-distance...}
                             {:pre [(colour-at from)]}
                             (into {}
                               (for [change-x '(-1 0 1)
                                     change-y (if (not= change-x 0)
                                                '(-1 0 1)
                                                '(-1   1))
                                     :let [farthest
                                           (some (fn [distance]
                                                   (let [
                                                         x (+ from-x (* change-x distance))
                                                         y (+ from-y (* change-y distance))]
                                                     (if (and 
                                                              (<= 0 x max-pos)
                                                              (<= 0 y max-pos))
                                                       distance)))
                                             (range max-pos -1 -1))]
                                     :when farthest]
                                 [[change-x change-y] farthest])))
          
          
          direction-distance-pos (fn [[from-x from-y :as from] [change-x change-y] distance]
                                   {:pre (colour-at from)
                                    :post (colour-at %)}
                                   [(+ from-x (* change-x distance))
                                    (+ from-y (* change-y distance))])
          
          positions-by-colour (fn [colour]
                                (for [x (range 0 width)
                                      y (range 0 width)
                                      :when (= (colour-at [x y]) colour)]
                                  [x y]))
          
          other-colour (fn [col] ({'b 'w 'w 'b} col))
          mine? (fn [pos] (= (colour-at pos) my-colour))
          hers? (fn [pos] (= (colour-at pos) (other-colour my-colour)))
          
          moves (fn []
                  (into {}
                    (for [free (positions-by-colour 'e)
                          :let [conquered (apply concat
                                            (for [[dir rad] (direction-radius free)
                                                  :when (> rad 1)
                                                  dis (range 2 (inc rad))
                                                  :while (hers? (direction-distance-pos free dir (dec dis)))
                                                  :let   [mine  (direction-distance-pos free dir      dis)]
                                                  :when  (mine? mine)]
                                              (map ;collect conquered pieces in this direction
                                                (partial direction-distance-pos free dir)
                                                (range 1 dis))))]
                          :when (seq conquered)]
                      [free (into #{} conquered)])))]
      (moves))))
      
;           0 1 2 3
(if false
  (reversi '[[e e e e]
             [e w b e]
             [e b w e]
             [e e e e] 'w]))   
{[1 3] #{[1 2]}, [0 2] #{[1 2]}, [3 1] #{[2 1]}, [2 0] #{[2 1]}}

(if false
  (reversi '[[e e e e]
             [e w b e]
             [w w w e]
             [e e e e] 'b]))  
{[3 2] #{[2 2]}, [3 0] #{[2 1]}, [1 0] #{[1 1]}}

;           0 1 2 3
(if false
  (reversi '[[e e e e]
             [e w b e]
             [w w b e]
             [e e b e] 'w]))       
{[0 3] #{[1 2]}, [1 3] #{[1 2]}, [3 3] #{[2 2]}, [2 3] #{[2 2]}}

(if false
   (reversi '[[e e w e]
              [b b w e]
              [b w w e]
              [b w w w] 'b]))  
{[0 3] #{[2 1] [1 2]}, [1 3] #{[1 2]}, [2 3] #{[2 1] [2 2]}}      

  




































