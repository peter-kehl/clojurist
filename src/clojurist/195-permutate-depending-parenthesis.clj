(require 'clojure.set)
;http://www.4clojure.com/problem/195 Parenthesis combinations
;(clojure.main/load-script "/home/pkehl/GIT/clojurist/src/clojurist/195-permutate-depending-parenthesis.clj")

; ((()))
; (()())
; (())()
; ()(())
; ()()()
; (((())))
; ((()()))
; ((())())
; ((()))()
; ...
; A transformation of the 1 below (non-tail recursive) method. This (transformed) method itself is non-recursive.
; The implementation is tail-recursive only to update the result. 
; Start with all openers on the left, all closers on the right.
; Each generation:
; 1. Iterate (skip) from the end, right to left. Count the balance = (- skipped-closers skipped-openers).
; 2. You reach the first "switchable" opener when it's an opener and (< 1 balance). If it's not the leftmost place,
;    swap the opener for a closer. Otherwise you finished (prev was the last generated result).
; 3. Right after the place where you just swapped an opener into a closer:
; First append all available openers. Then append all available closers (plus the one you swapped).
; 4. That gives you a result. Repeat for the next result.
(def parens-flat
  ; - prev: the last result
  ; binary 1 for an opener (, 0 for a closer )
  ; Return a seq. of complete solutions as numbers that in binary are of full-length (2x n). No need to return as a set
  ; because the results are already unique.
  (fn [n-pairs]
    (let [n-pairs*2 (* 2 n-pairs)]
      (letfn [(digits [numb]
                {:pre [(number ?numb)]}
                (vec (reverse (for [i (range 0 n-pairs*2)]
                                (bit-test numb i)))))
              (count-digits [numb digit]
                {:pre [(number? digit)]}
                (count (filter (partial = digit) (digits numb))))
              (generate [prev]
                ;(assert (and (<= 0 openers) (<= 0 closers) (<= 0 diff) (= diff (- closers openers))))
                ;(assert (or (zero? diff) (pos? closers)))
                (assert (= (count-digits prev 1) (count-digits prev 0) n-pairs))
                (let [swap-point (loop [i 0]
                                   (if (bit-test prev i)
                                     1
                                     (recur (dec i))))]
                  1)                   
                
                (let [left<< (bit-shift-left left 1) #_calculate-left<<-even-if-not-needed-as-the-extra-check-at-every-level-costs-more
                      results (into ;concat was around 1.5x slower than into!
                                (if (pos? openers)
                                  (level (bit-set left<< 0) (inc diff) (dec openers)      closers) ; left (
                                  ())
                                (if (pos? diff) ;that implies (pos? closers)
                                  (level          left<<    (dec diff)      openers  (dec closers)) ;left )
                                  ; every leaf finishes with a closer paren )
                                  (if (zero? closers)
                                    (list left)#_all-parens-used
                                    ())))]
                  results))
              (humanise [number]
                (clojure.string/replace
                  (clojure.string/replace (java.lang.Long/toBinaryString number) ;toBinaryString is faster 40ms than clojure.pprint/cl-format 308ms.
                    \1 \()
                  \0 \)))]
        (into #{}
          (if (zero? target-n)
            ()
            (map humanise
              (level 0 0 target-n target-n))))))))

; a recursive (non-tail) method. Deep search-like, back trace. Iterate from the left. Bitwise operations. Pass:
(def parens-stack
; - left: the result so far
; - diff - the unclosed expression depth/the number of currently unclosed openers:
  ;    0 if left has openers & closers balanced, positive if left has more openers.
;   (depth could be determined as (- closers openers). However, we pass it as a param, to save the calculation.) 
; - the number of available (leftover) parens openers & closers
; binary 1 for parens opener (, 0 for for parens closer )
  ; Return a seq. of complete solutions as numbers that in binary are of full-length (2x n). No need to return as a set
  ; at every level, because the results are already unique.
  (fn [target-n]
    (letfn [(level [left diff openers closers #_num-of-available-openers-and-closers]
              ;(assert (and (<= 0 openers) (<= 0 closers) (<= 0 diff) (= diff (- closers openers))))
              ;(assert (or (zero? diff) (pos? closers)))
              (let [left<< (bit-shift-left left 1) #_calculate-left<<-even-if-not-needed-as-the-extra-check-at-every-level-costs-more
                    results (into ;concat was around 1.5x slower than into!
                              (if (pos? openers)
                                (level (bit-set left<< 0) (inc diff) (dec openers)      closers) ; left (
                                ())
                              (if (pos? diff) ;that implies (pos? closers)
                                (level          left<<    (dec diff)      openers  (dec closers)) ;left )
                                ; every leaf finishes with a closer paren )
                                (if (zero? closers)
                                  (list left)#_all-parens-used
                                  ())))]
                results))
            (humanise [number]
               (clojure.string/replace
                 (clojure.string/replace (java.lang.Long/toBinaryString number) ;toBinaryString is faster 40ms than clojure.pprint/cl-format 308ms.
                   \1 \()
                 \0 \)))]
      (into #{}
        (if (zero? target-n)
         ()
         (map humanise
           (level 0 0 target-n target-n)))))))
; the following is the last test from 4clojure. It takes 1.9-2.4sec - too slow!
; But it's slow even if you replace (map humanise...) with (map identity...): 2.2-2.6sec!
; Replacing (man humanise ...) with calling (humanise...) at the leaf level from (level) saves only c.a. 100ms.
;(time (= (nth (sort (parens-stack 12)) 5000) "(((((()()()()()))))(()))"))

    

; current depth
; distance to the match
; ()()
; 1010
; 1111

; (())
; 1210
; 4114

; ((()))
; 123210
; 531135

; (()())
; 121210
; 611116

; ()()()
; 101010
; 111111

; (())()
; 121010
; 411411

; ()(())
; 114114

; (((()())(())))
; 12343432343210
; EC6111164114CE
; C=12, E=14
