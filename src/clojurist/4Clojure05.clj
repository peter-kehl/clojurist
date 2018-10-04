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

    
    
    






























