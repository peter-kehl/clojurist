(require 'clojure.set)
(require 'clojure.pprint)
(require 'dbg.main)
;(clojure.main/load-script "/home/pkehl/GIT/clojurist/src/clojurist/dbg.clj")

;http://www.4clojure.com/problem/140
(def veitch
  (fn [rules-orig]
    (let [upper-case? (fn [sym] (let [st (str sym)]
                                  (= (clojure.string/upper-case st) st)))
          full-rules (into #{} ;A set of maps, each {upper-case-symbol boolean, ...} for all fields (based on symbols).
                       (map
                         (fn [rule]
                           (into {} (map
                                      (fn [sym]
                                        (let [turned-on (upper-case? sym)]
                                          [(clojure.string/upper-case sym) turned-on]))
                                      rule)))
                         rules-orig))
          ;_ (println "full-rules:" full-rules)
          fields (map ;a seq. of upper-case strings of all possible symbols. Easy, because
                   ;every set in sets-orig contains all symbols (either on/off - uppercase/lowercase).
                   #(clojure.string/upper-case %)
                   (first rules-orig))
          ;_ (println "fields:" fields)
          ;In following map-based structures, upper-case strings map to boolean.  Because of false, use contains? or nil? to detect relevance (presence) of a field. Don't use (map-instance field-upper-case-name) to determine presence of  field.
          ;;2D map of sets of maps: { upper-case { boolean-for-that-symbol #{set of maps, one map per each rule having that boolean-for-that-symbol as a value for symbol upper-case, each map in the same format as entries in full-rules, but excluding that upper-case field itself: {other-uppercase boolean, ...}, ...}
          field-value-others (reduce 
                               (fn [res full-rule] ;For values true/false for every field and for every full rule, generate partial rules (that don't contain the same field). Store them per that "routing" field and per true/false.
                                 (loop [routing-field (first fields)
                                        next-fields (next fields)
                                        res res]
                                      (let [routing-value (full-rule routing-field)
                                            partial-rules (get-in res [routing-field routing-value] #{})
                                            new-res (assoc-in res [routing-field routing-value] (conj partial-rules (dissoc full-rule routing-field)))]
                                        (if next-fields
                                          (recur (first next-fields) (next next-fields) new-res)
                                          new-res))))
                               {}
                               full-rules)
          ;_ (do (println "field-value-others:") (clojure.pprint/pprint field-value-others))
          ;A map {full rule: full rule}. A help structure for a startpoint below.
          full-rules-to-themselves (into {} (map #(vector % %) full-rules))
          ;Where ((field-value-others chosen-upper-case) true) and ((field-value-others chosen-upper-case) false)
          ;contain *SOME* equal entries, we can
          ;simplify that rule by eliminating that chosen-upper-case from those equal entries (connected rules). That shortens the original rule (it removes chosen-upper-case.) 
                                        ;The same rule can be shortened several times. On those successive shortening operations, the involved partial rules from field-value-others don't know that the rule was shortened already. Hence, as we shorten rules, we need to keep a track of the original rule for each shortened one. The starting point is full-rules-to-themselves.
          full-rules-to-short (reduce (fn [full2short field]
                                        (let [partials-true  (get-in field-value-others [field  true] #{})
                                              partials-false (get-in field-value-others [field false] #{})
                                              ;Which entries (maps) in partials-true and partials-false are equal? For those rules eliminae field altogether.
                                              partials-shared (clojure.set/intersection partials-true partials-false)]
                                          (reduce (fn [f2s partial]
                                                    (let [full-true  (assoc partial field true)
                                                          full-false (assoc partial field false)]
                                                      (assoc f2s full-true partial full-false partial)))
                                                  full2short partials-shared)
                                           #_(if false ; *SOME* of (= partials-true partials-false)
                                            1
                                            full2short #_no-shortening-of-this-field)))
                                      full-rules-to-themselves fields)
          ;_ (do (println "full-to-short:") (clojure.pprint/pprint full-rules-to-short))
          set-of-maps (into #{} (vals full-rules-to-short))]
      (into #{} (map (fn [mp]
                       (into #{} (map (fn [[field value]]
                                        (if value
                                          field
                                          (clojure.string/lower-case field)))
                                      mp)))
                     set-of-maps))
      )))
      
    
