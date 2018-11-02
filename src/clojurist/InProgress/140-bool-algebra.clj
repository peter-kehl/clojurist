(require 'clojure.set)
;(clojure.main/load-script "/home/pkehl/GIT/clojurist/src/clojurist/dbg.clj")

;http://www.4clojure.com/problem/140
(def veitch
  (fn [rules-orig]
    (let [upper-case? (fn [sym] (let [st (str sym)]
                                  (= (clojure.string/upper-case st) st)))
          full-rules (into #{} ;A set of maps, each {upper-case-symbol boolean, ...} for all fields (based on symbols). Because of false, use contains? or nil? to detect relevance (presence) of a field.
                       (map
                         (fn [rule]
                           (into {} (map
                                      (fn [sym]
                                        (let [on (upper-case? sym)]
                                          [(clojure.string/upper-case sym) on]))
                                      rule)))
                         rules-orig))
          fields (map ;a seq. of upper-case strings of all possible symbols. Easy, because
                   ;every set in sets-orig contains all symbols (either on/off - uppercase/lowercase).
                   #(clojure.string/upper-case %)
                   (first rules-orig))
          ;;2D map of sets of maps { upper-case { boolean-for-that-symbol #{set of maps, one map per rule having that boolean-for-that-symbol: {other-uppercase boolean, ...}, ...}
          field-value-others (reduce 
                               (fn [res full]
                                 (for [rule full-rules]
                                   (assoc-in res (list field (clojure.string/upper-case)))))
                               #{}
                               full-rules)
          ;Where ((field-value-others chosen-upper-case) true) and ((field-value-others chosen-upper-case) false)
          ;contain some equal entries, we can
          ;simplify that rule by eliminating that chosen-upper-case from those equal entries (connected rules). 
          _ 1]
      1)))    
      
    
