(require 'clojure.set)

;http://www.4clojure.com/problem/195 Parenthesis combinations
(def parens-orig
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
(def parens
  (fn [target-n]
    ; (((( ))))
    ; n =>> n+1: ( around ), () before, with () in - at any index, after ()
    ; NOT handling: with in n - but ( around a subset ) of n - at any possible index
    ; - if the result is whole within (), then such a result can be accomplished by the above rule: ( around )
    ; - otherwise the result is within 2+ () groups. Each was generated at lower level. It should have been
    ;   embraced in another top level () pair as per above rules.
; Use bitwise operations. 1 stands for \( because there has to be \( on the left
; (it would be more difficult with leading 0's on the left). Keep results as numbers - this makes a set #{} faster.
; Only once we collect all numbers, transform to strings. Then replace 1 with \(, 0 with \).
; (clojure.pprint/cl-format nil "2r~6,'0',B" (bit-shift-left 1N 63)) - bit operations not supported for BigInt. Hence max 63 => max 31 pairs of parens.
; (.toString (.toBigInteger 10N) 2)
; 4Clojure.com runs CLJ 1.4, which doesn't have unsigned-bit-shift-right. But we don't need it anyway.
    (let [count-digits (fn [number #_unsigned] #_count-binary-digits-from-leading-1
                         ;{:pre [(< 0 number)]}
                         (loop [shifted number
                                width 0]
                           (if (= shifted 0)
                             width
                             (recur (bit-shift-right shifted 1) (inc width)))))]
          ;testing (assert (= (count-digits 2r11001) 5))
      
      (if (zero? target-n)
          #{""}
          (into #{}
            (map
              (fn [number]
                  (clojure.string/replace
                    (clojure.string/replace (java.lang.Long/toBinaryString number) ;toBinaryString is faster 40ms than clojure.pprint/cl-format 308ms.
                      \1 \()
                    \0 \)))
              ;using a function #({"1" "(" "0" ")"} %) with string/replace was slower: 430ms at target-n=9. 2x char replace: 308ms.
              (let [expand (fn [grp]
                             (let [grp-width (count-digits grp)]
                               ;(assert (= (* 2 prev-n) grp-width) (str "prev-n: " prev-n ", grp: " grp " and in binary: " (clojure.pprint/cl-format nil "~,'0',B" grp)))
                               (into
                                 (list
                                   ; \( grp \) == 1 grp 0
                                   (bit-shift-left (bit-set grp grp-width) 1)
                                   
                                   ; grp \( \)
                                   (bit-or
                                     (bit-shift-left grp 2)
                                     2r10))
                                 
                                 ;cut grp into two parts, starting from the right. Inject () in between.
                                 (loop [i 0
                                        ;left and left-new are "local," i.e. to contribute towards the result,
                                        ;they need to be shifted to the left by i number of bits.
                                        left (bit-shift-left grp 2) ;2 bits are an extra for injecting a () pair that will start at the rightmost
                                        right 0
                                        res ()]
                                   ;(println "left:             " (clojure.pprint/cl-format nil "~,'0',B" left))
                                   ;(println "right:            " (clojure.pprint/cl-format nil "~,'0',B" right))
                                   ;(assert (zero? (bit-and left 2r11))) ;ensure 2 blanks on the right
                                   (if (= i grp-width)
                                     res
                                     (let [left-new (bit-clear (bit-shift-right left 1) 1) ;shift, but keep the rightmost 2 bits blank
                                           ;_ (println "left-new:         " (clojure.pprint/cl-format nil "~,'0',B" left-new))
                                           left-new-and-pair (bit-or left-new 2r10)
                                           ;_ (println "left-new-and-pair:" (clojure.pprint/cl-format nil "~,'0',B" left-new-and-pair))
                                           right-new (if (bit-test left 2)
                                                       (bit-set right i)
                                                       right)
                                           ;_ (println "right-new:        " (clojure.pprint/cl-format nil "~,'0',B" right-new))
                                           grp-new (bit-or (bit-shift-left left-new-and-pair (inc i)) right-new)
                                           ;_ (println "grp-new:          " (clojure.pprint/cl-format nil "~,'0',B" grp-new))
                                           res-new (cons grp-new res)]
                                       (recur (inc i) left-new right-new res-new)))))))]
                (loop [prev #{2r10}
                       prev-n 1]
                  (if (= target-n prev-n)
                    prev
                    ;the above inner (loop) is a bitwise rewrite of the following (for), but reverse, and passing on helper values
                    ;(for [i (range 0 (count grp))]
                    ;  (subs grp 0 i) "()" (subs grp i)))))]
                    ;(str (subs grp 0 i) "()" (subs grp i)))))]
                    (recur
                      (into #{} ;about the same speed as apply conj #{}...
                        (apply concat ;% faster than: reduce into #{}
                          (#_dbgf #_"map" map expand prev)))
                      (inc prev-n)))))))))))

; The first solution, rewritten to generate a set of seq first, then make them strings.
; That saves creation of string duplicates. Unfinished: missing (str ...). But it was 3.5x slower than with strings!
; n=12 => 2.1 sec with strings; 7.4sec with vectors!
(def parens-str
  (fn [target-n]
    (loop [prev #{[]}
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
; NOT ALL RESULTS!
(def parens2
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

; The following runs out of heap memory even at (parens 10), very slow at n=7!
; the above was not covering all: n=3 -> only  #{"(()())" "((()))" "()()()" "(())()"}
; missing (()()) -> ()(()) <-- when moving to the right the 2nd/3rd/farther... rightmost opener first
; -> then "jump over" the other rightmost opener(s)
; - treat a consecutive group of openers (( or ((( or ((((... as the same - it doesn't matter which of them you move to the right
; --- hence act only on 2nd/3rd/4th... openers ( that have a closer ) immediately right from them
(def parens3
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
#_(parens 3)
#_(count (parens 10))
#_(time (= (nth (sort (parens 12)) 5000) "(((((()()()()()))))(()))")) ;2.1 sec with strings; 7.4sec with vectors!

