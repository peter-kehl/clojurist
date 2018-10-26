(require 'clojure.set)
;http://www.4clojure.com/problem/195 Parenthesis combinations

; a recursive (non-tail) method. Deep search-like. Iterate from the left. Bitwise operations. Pass the number of opened parens.
; binary 1 for (, 0 for )
(def parens-stack
  (fn [target-n]
    (letfn [(level [left depth openers closers #_num-of-available-openers-and-closers]
               (if (not (zero? openers))
                 (level)
                 ())
               (if (not (zero? closers))
                 1))])))
                  
    

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
