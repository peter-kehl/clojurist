
(doc map)
(doc doc)

(map (fn [& _] true) #{1 2 3})
(filter #(do [%] true) [:anything :here])

;(map (fn [] true) #{1 2 3})
;(filter #(do true) [:anything :here])
[1 #_anything 2]

(if false
  (do
   (let [fact (fn [n] (
                          (loop [n n product 1] (if (<= n 1) product (recur (dec n) (* n product))))))]  (fact 4))
           
   (let [fact (fn [n] (
                          (loop [n n product 1]
                            (if (<= n 1)
                              product
                              (recur (dec n) (* n product))))))]  (fact 4))))

  

(let [fact (fn [n] ;Don't have an extra ( here. Otherwise you'll get: Long can't be cast to class clojure.lang.IFn!
               (loop [n n product 1]
                 (if (<= n 1)
                   (do (println product) product)
                   (do (println (str "n " n ", product " product)) (recur (dec n) (* n product))))))] (fact 4))

(if false nil)

( (fn [x] {:post [(= % x)]} x) :anything) ;=> :anything - OK 
;( (fn [x] {:post [(= % x)]} :incorrect) :anything) ;=> nothing shown as a result in InstaREPL
*assert* ;=> true

( (comp :street :address) {:address {:street "1 Main"}})
(let [address-part (fn [address-field-keyword]
                     (comp address-field-keyword :address))]
                     
  ( (address-part :street) {:address {:street "1 Main"}}))


(= (int 0.6) 0) ; round down!
((comp int inc #(/ % 2)) 10)

( (fn [text]
    (reduce
     #_applier_of_each_function_from_vector (fn [text transformation] (transformation text))
     text
     #_vector_of_transformations [clojure.string/upper-case #(str "Hi " %)]))
  "john")
