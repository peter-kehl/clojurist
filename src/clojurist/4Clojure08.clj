(use 'clojure.set)

;http://www.4clojure.com/problem/152 LAtin Squares
(def latin
  (fn [vecs-orig]
    (let [height (count vecs-orig)
          max-x (dec height)
          width (apply max (map count vecs-orig))
          max-y (dec width)
          max-size (min width height)
          
          ; (view ...) used to throw on wrong index(es) - so that get-square can catch it easily. But 4clojure refuses (catch...)
          view (fn [shifts x y] ;nil if not present
                 (get (vecs-orig x) (- y (nth shifts x))))
          
          square? (fn [square]
                    (let [size (count square)]
                      (every? (fn [row] (= (count row) size)) square)))
          pprint-one-square (fn [square]
                              (count (map #(println %) square))) ;(count ...) because (map...) is lazy
          pprint-squares (fn [squares]
                           (count (map
                                    #(pprint-one-square %)
                                    squares))
                           (println)) ;(count ...) because (map...) is lazy
          
          axis-ranges (vec (map ;axis-ranges[] is vector: size => (range 0 size).
                             #(range 0 %)
                             (range 0 (inc (max width height))))) ;even though we use size >=2, this must start at 0 so it's indexable
          ;_ (println "axis-ranges" axis-ranges)
          get-square (fn [shifts top-left-x top-left-y size] ;return nil if no such square
                       ;{:post [(or (nil? %) (square? %))]}
                       (let [axis-range (axis-ranges size)
                             result (for    [x axis-range]
                                      (for  [y axis-range
                                             :let [cell (view shifts (+ x top-left-x) (+ y top-left-y))]
                                             :while cell]
                                        cell))]
                         ;(dbg-println "potential result:") (pprint-one-square result)
                         (if (every?
                               ;if cell was nil, it was *not* appended to the row (due to the above :while). Such a row is shorter.
                               (fn [row] (= (count row) size))
                               result)
                           result
                           nil)))
          ; a list of sequences, each cell containing a shift (0 or higher) of its respective vector (row) in vecs-orig[]. 
          groups-of-shifts (letfn [(sub-shifts-since-level [level]
                                     (let [results-below (if (< level max-x) #_alternativ-to-memoize
                                                           (sub-shifts-since-level (inc level))
                                                           :unused)]
                                       (apply concat
                                         (for [shift (axis-ranges (inc (- width (count (vecs-orig level)))))]
                                           (if (= level max-x)
                                             [[shift]]
                                             (map
                                               #(cons shift %) ;alternative: (partial cons shift)
                                               results-below))))))]
                             (sub-shifts-since-level 0))
          ;_ (clojure.pprint/pprint groups-of-shifts)
          
          ;return a set of items, if slices (rows) form a horizontally-latin square; false otherwise
          horizontal-latin? (fn [slices]
                              (let [first-as-set (into #{} (first slices))]
                                (if (and (= (count first-as-set) (count slices))
                                         (every?
                                           #(= (into #{} %) first-as-set)
                                           (rest slices)))
                                  first-as-set
                                  false)))
          ;this would need some of the above anyway: (apply = (map (partial into #{}) slices))
          
          latin? (fn [square]
                   ;{:pre [(square? square)]}
                   (let [horizontal (horizontal-latin? square)]
                     (and horizontal
                          (let [columns (for [col-index (axis-ranges (count square))] ;rotate columns into rows
                                          (map #(nth % col-index) square))]
                            (= (horizontal-latin? columns) horizontal)))))
          x-range (axis-ranges height)
          y-range (axis-ranges width)
          ;Get all possible squares. High-level optimisation: Keep a track of *slices* (i.e. consecutive parts) of length 2 or more of
          ;already processed shifts. Skip collecting squares of the size & location that fits into
          ;those already processed shifts. (This optimisation is partial, as two different shift groups may shift
          ;two or more neighbouring rows by the same shift.)
          ;prev-shift-slices set of maps #{ {index-of-row shift-for-that-row...}... } for two or more consecutive rows.
          ;If vecs-orig[] has more than two rows, then any shift generates multiple entries in prev-shift-slices #{},
          ;to cover all combinations of two or more consecutive rows.
          ;Because we're caching/skipping based on shifts, each (loop) iteration processes one shift completely.
          squares (loop [prev-shift-slices #{}
                         prev-shifted-rows {} ;2-dimensional array {row-index-x {shift-of-that-row row-of-cells ..} ..}
                         shifts-leftover groups-of-shifts
                         res #{}]
                    (let [shifts (first shifts-leftover)
                          ; seq. of seqs, where first cell is a seq. with 2-D indexes like for prev-shifted-rows, and second cell is a value (shifted row). This structure is easy to (apply assoc-in result-map ...)
                          shifted-rows-new (for [x x-range
                                                  :let [shift (nth shifts x)
                                                        old (get-in prev-shifted-rows (list x shift))]
                                                  :when (nil? old)]
                                              (let [row-orig (vecs-orig x)
                                                    row (vec (concat
                                                               (repeat shift nil)
                                                               row-orig
                                                               (repeat (- width shift (count row-orig)) nil)))] ;repeat accepts negative n => empty seq ()
                                                (list (list x shift) row)))
                          shifted-rows-next (reduce
                                              (fn [res x-shift-row]
                                                (apply assoc-in res x-shift-row))
                                              prev-shifted-rows
                                              shifted-rows-new) 
                          ;view is a 2-dimensional height X width vec[] of vecs-orig[], a representation of vecs-orig[] with applied shifts[]
                          view (vec (for [x x-range]
                                      (get-in shifted-rows-next (list x (nth shifts x)))))
                          ;specific per size, because squares of different size (generally) don't share parts
                          pack-shift-slice (fn [top-x size] (into {}
                                                              (map
                                                                (fn [x] [x (nth shifts x)])
                                                                (range top-x (+ top-x size)))))
                          res-and-shifted-slices-new
                          (for  [size (range 2 (inc max-size))
                                 :let [top-x-to-packed-shift-slice ;List of pairs [top-x (pack-shift-slice ...)]
                                       (for [top-x (axis-ranges (inc (- height size)))
                                             :let [shift-slice (#_dbgf pack-shift-slice top-x size)]
                                             ;_ (if (prev-shift-slices shift-slice) (println "Skipping"))
                                             ;_ (println 'prev-shift-slices prev-shift-slices)
                                             :when (not (prev-shift-slices shift-slice))]
                                         [top-x shift-slice])
                                       top-left-y-range (axis-ranges (inc (- width size)))] ;excluding the last, since squares have size >=2
                                 [top-left-x shift-slice] top-x-to-packed-shift-slice]
                            [(for [top-left-y top-left-y-range
                                   :let [;_ (println "shifts" shifts "top [" top-left-x top-left-y "size" size)
                                         square (#_dbgf get-square shifts top-left-x top-left-y size)]
                                   :when square] ;TODO 0. start with the rightmost column of the square (item set reused for -> #2) 1. add only if latin 2. then check +size-1 column to the right. Perform the whole check only if both columns have same item set.
                               ;                    \-> pass the reference column and its position, or the index set of the rest of the columns, as a parameter to latin?
                               square)
                             shift-slice])
                          res-new (apply concat
                                    (map first res-and-shifted-slices-new))
                          res-next (into res res-new)
                          shift-slices-new (map second res-and-shifted-slices-new)
                          shift-slices-next (into prev-shift-slices shift-slices-new)
                          shifts-leftover-next (next shifts-leftover)]
                      (if shifts-leftover-next
                        (recur
                          shift-slices-next
                          shifted-rows-next
                          shifts-leftover-next
                          res-next)
                        res-next)))
          ;_ (pprint-squares squares)
          latin-squares (distinct
                          (filter latin? squares))]
      
      (into {}
        (map
          (fn [[size sqs]]
            [size (count sqs)])
          (group-by count latin-squares))))))
(if false
  (latin [[8 6 7 3 2 5 1 4]
          [6 8 3 7]
          [7 3 8 6]
          [3 7 6 8 1 4 5 2]
          [1 8 5 2 4]
          [8 1 2 4 5]]))
(if false
; indexes  0 1 2 3 4 5
  (latin [[3 1 2]
          [1 2 3 1 3 4]
          [2 3 1 3]]))
; 2 squares of size 2, both in the 2nd and 3rd row:
; 31   13
; 13   31

(if false
  (latin '[[A B C D]
           [A C D B]
           [B A D C]
           [D C A B]]))        
(if false
  (latin '[[A B C D E F]
           [B C D E F A]
           [C D E F A B]
           [D E F A B C]
           [E F A B C D]
           [F A B C D E]]))  
(if false
  (latin '[[A B C D]
           [B A D C]
           [D C B A]
           [C D A B]]))
