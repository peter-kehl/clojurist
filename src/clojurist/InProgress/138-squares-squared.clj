;(clojure.main/load-script "/home/pkehl/GIT/clojurist/src/clojurist/dbg.clj")

(def squares-sq (fn [num-from num-max]
  (let [nums (take-while #(<= % num-max)
               (iterate #(* % % #_Math.pow-is-for-double-only) num-from))
        ;_ (println "nums:" nums)
        digits (for [n nums
                     d (str n)] d)
        _ (println :digits digits)
        matrix? map?
        ;mx is a matrix-like map of maps, with indexes that can be negative: relative to the start point.
        at (fn [mx x y] {:pre [(or (matrix? mx) (seq? mx))]} #_returns-nil-if-not-set (get-in mx [x y]))
        ;set-at (fn [mx x y value] (assoc-in mx [x y] value))
        ;directions as [delta-x delta-y], in coordinates where x is a row, y is a column, [0 0] is the top left corner. D down, U up, R right, L left:
        d-r [1 1], d-l [1 -1], u-l [-1 -1], u-r [-1 1]
        ;directions listed in order, each 45degrees to the right after the previous one.
        directions [d-r d-l u-l u-r]
        directions-reversed (reverse directions)
        dir? (fn [[delta-x delta-y]] (and (<= -1 delta-x 1) (<= -1 delta-y 1) (not= delta-x delta-y 0)))
        turn (fn [prev-dir] {:pre [(dir? prev-dir)]};previous direction => new direction 45degrees to the right
               (if (= prev-dir u-r)
                 d-r
                 (first (for [dir directions-reversed
                              :while (not= dir prev-dir)]
                          dir))))
        place? (fn [[x y :as pl]] (and (= (count pl) 2) (number? x) (number? y)))
        move (fn [[x y :as pl] [x-delta y-delta :as dir]] {:pre [(place? pl) (dir? dir)]}
               [(+ x x-delta) (+ y y-delta)])
        place-dir (fn [place-prev dir-prev] ;return [new-place new-dir] where new-dir may be the same as dir-prev
                        {:pre [(place? place-prev) (dir? dir-prev) #_(or (true? over-two?) (false? over-two?))] :post [(place? (first %)) (dir? (second %))]}
                        (let [dir-right (turn dir-prev)
                              place-prev-right-neighbour (move place-prev dir-right)]
                            (if place-prev-right-neighbour
                              (let [place-prev-direct-neighbour (move place-prev dir-prev)]
                                [place-prev-direct-neighbour dir-prev]))
                            [place-prev-right-neighbour dir-right]))
        new-row sorted-map ;sorted only for debugging, in production it can be hash-map
        entry? #(or (and (char? %) (<= 48 (int %) 57)) (= % \*))
        fill-in (fn [places [x y :as place] value] {:pre [(matrix? places) (place? place) (entry? value)] :post [(= ((% x)y) value)]} ;like assoc-in, but using new-map
                  (let [row (get places x (new-row))
                        row-updated (assoc row y value)]
                    (println :fill-in :place place :value value)
                    (assoc places x row-updated)))
        ;start with direction up to the right u-r, because place-and-dir will turn it to d-r after the 1st digit.
        ;To complete a square, we need an even number of turns (ignoring the very first "turn" from starting direction u-r).
        places (let [[places-for-digits place-last-digit dir-last-digit num-of-turns]
                     (loop [places (sorted-map 0 (sorted-map 0 (first digits)))
                            place-last [0 0]
                            dir-last u-r
                            num-of-turns -1
                            digits-leftover (next digits)]
                       (println :places) (println places)
                       (println :place-last place-last :dir-last dir-last :num-of-turns num-of-turns :digits-leftover digits-leftover)
                       (if digits-leftover
                           (let [[place dir] (place-dir place-last dir-last)
                                 same-dir? (= dir-last dir)
                                 num-of-turns-new (if same-dir? num-of-turns (inc num-of-turns))
                                 _ (println digits-leftover)
                                 places-new (fill-in places place (first digits-leftover))]
                             (recur places-new place dir num-of-turns-new (next digits-leftover) ))
                           [places place-last dir-last num-of-turns])
                       )
                     _ 1 ;when filling up with stars *, after the very last (even numbered) turn fill only places that have a neihbour on the right.
                     _ (if false #_TODO-based-on-num-of-turns
                           1 ;fill up with stars
                           places-for-digits)]
                 places-for-digits)
        _ (println :places places)
        rows-strings (let [[min-x max-x min-y max-y] (reduce (fn [[min-x max-x min-y max-y] [x y]]
                                                               [(min min-x x) (max max-x x) (min min-y y) (max max-y y)])
                                                             [10 -10 10 -10]
                                                             (for [[x row] places, [y _] row] [x y]))
                           _ (println "min-x" min-x "max-x" max-x "min-y" min-y "max-y" max-y)
                           rows-with-spaces (for [x (range min-x (inc max-x))]
                                               (apply str
                                                  (for [y (range min-y (inc max-y))]
                                                    (get-in places [x y] \space)
                                             )))]
                        rows-with-spaces)
        _ (doseq [row rows-strings] (println (str row \|)))
        ] rows-strings)))
