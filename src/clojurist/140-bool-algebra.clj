(require 'clojure.set)
;(clojure.main/load-script "/home/pkehl/GIT/clojurist/src/clojurist/dbg.clj")

(def veitch
  (fn [sets-orig]
    ; map of maps {field: boolean}. Because of false, use contains? or nil? to detect relevance (presence) of a field.
    (let [maps-orig (into #{} (map
                                (fn [set-orig]
                                  (into {} (map
                                             #()
                                             set-orig)))
                                sets-orig))])))
    
