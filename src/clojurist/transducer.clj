(clojure.main/load-script "/home/pkehl/GIT/clojurist/src/clojurist/dbg.clj")

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

; https://labs.uswitch.com/transducers-from-the-ground-up-the-essence
(dbgf "transduce (map inc) conj (range 10)" transduce
  (let [map-inc (map inc)] #(dbgf "apply map-inc" apply map-inc %&))
  #(dbgf "conj" apply conj %&)
  (dbgf range 10)) ;TODO dbg macro to wrap a function (outside of a form) w/o params
; - where does (transduce) get the initial vector []?!!
;; same as:
(reduce ((map inc) conj) [] (range 10))

(sequence (map inc) (range 10))
(into [] (map inc) (range 10))
;-----------------------------------------

;https://labs.uswitch.com/transducers-from-the-ground-up-the-practice/
(def inc-and-filter (comp (map inc) (filter odd?))) ;<<<<<
(def special+ (inc-and-filter +))
(special+ 1 1) ;; 1 ;the second argument is incremented and then conditionally added
(special+ 1 2) ;; 4

(transduce (comp (map inc) (filter odd?)) + (range 10)) ;; 25

;(require 'clojure.core.async) ;<<<<

(defn egypt-mult-upto-partition [x y]
  (->> (interleave
         (iterate #(quot % 2) x)
         (iterate #(* % 2) y))
       (partition-all 2)
       (take-while #(pos? (first %)))))
(egypt-mult-upto-partition 640 10)

; ---- Could not locate clojure/core/async__init.class

(def ^:dynamic bind-var false)
(defn bind-test []
  bind-var)
(bind-test)
(binding [bind-var true]
  (bind-test))

(
  (binding [bind-var true]
    #(bind-test)))
; -> false

; Lazy is buffered in batches
(take 2 (map #(println %) (range 95))) ;-it prints 0..31 - lazy but ahead
; Lazy results are really cached: following prints same log only once
(let [lazy (map #(println %) (range 95))] (take 1 lazy) (take 3 lazy))





























; Special
(((map inc) +) 1) ;=> 1
(((map inc) +) 1 2) ;=> 4
(if false
 (((map inc) +) 1 2 3)) ;=> Wrong number of args (2) passed to: clojure.core/inc

;TODO dbg:
;namespace
;replace #object[...] with the most recent scope
;-- replace clojure.core$xyz without any farther $fn_... appended =>> clojure.core/xyz. Any farther $fn__... or $user-given-name =>> new function. 
;print "Args for...." only if there's an inner standard output
;(dbgf ...) prints target (formal) param name for each actual param value
;--- meta?
; node.js: write to a file?
