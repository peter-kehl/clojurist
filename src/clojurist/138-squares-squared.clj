(clojure.main/load-script "/home/pkehl/GIT/clojurist/src/clojurist/dbg.clj")

(fn squares-sq [num-from num-max]
  (let [nums (take-while #(<= % num-max)
               (iterate #(* % % #_Math.pow-is-for-double-only) num-from))
        _ (println "nums:" nums)]))
