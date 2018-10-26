(require 'clojure.set)
;(clojure.main/load-script "/home/pkehl/GIT/clojurist/src/clojurist/dbg.clj")

;http://www.4clojure.com/problem/140
(def veitch
  (fn [sets-orig]
    ; map of maps {field: boolean}. Because of false, use contains? or nil? to detect relevance (presence) of a field.
    (let [upper-case? (fn [sym] (let [st (str sym)]
                                  (= (clojure.string/upper-case st) st)))
          full-maps (into #{} ;a set of maps, each {upper-case-symbol boolean} for all symbols
                      (map
                        (fn [set-orig]
                          (into {} (map
                                     (fn [sym]
                                       (let [on (upper-case? sym)]
                                         [(clojure.string/upper-case sym) on]))
                                     set-orig)))
                        sets-orig))
          fields (map ;a seq. of upper-case strings of all possible symbols. Easy, because
                      ;all sets in sets-orig contain all symbols (either on/off - uppercase/lowercase).
                   #(clojure.string/upper-case %)
                   (first sets-orig))
          field-value-others (reduce ;3D map { upper-case { boolean-for-that-symbol {other-uppercase boolean...}}}
                               (fn [res full]
                                 ;iterate over fields
                                 (assoc-in res (list (clojure.string/upper-case))))
                               #{}
                               full-maps)]
      1)))    
      
    
