(reduce ((filter odd?) +)   [1 2 3 4 5])
(reduce ((filter odd?) +) 0 [1 2 3 4 5])
; 9

(reduce ((filter odd?) conj) [] [1 2 3 4 5])
(transduce (filter odd?) conj [] [1 2 3 4 5])
; [1 3 5]

(transduce (comp (map inc) (filter odd?)) conj [] [1 2 3 4 5])
; [3 5]

(transduce (map -) + [1 2 5 3])
; -11

; ???!
(transduce (map /) + [1 2 5 3])
;->61/30
