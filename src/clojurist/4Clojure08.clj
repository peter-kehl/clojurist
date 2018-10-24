(use 'clojure.set)

;http://www.4clojure.com/problem/152 LAtin Squares
(def latin
  (fn [vecs-orig]
    (let [height (count vecs-orig)
          max-x (dec height)
          width (apply max (map count vecs-orig))
          max-y (dec width)
          max-size (min width height)
          ; Can't throw on wrong index(es) - so that upper function can catch it easily. Why? This 4clojure problem refuses (catch...)

          pprint-one-square (fn [square]
                              (count (map #(println %) square))) ;(count ...) because (map...) is lazy
          pprint-squares (fn [squares]
                           (count (map
                                    #(pprint-one-square %)
                                    squares))
                           (println)) ;(count ...) because (map...) is lazy
          
          axis-ranges (vec (map ;axis-ranges[] is a vector: size => (range 0 size).
                             #(range 0 %) ;making it a vector will slow the usages down!
                             (range 0 (inc (max width height))))) ;even though we use size >=2, this must start at 0 so it's indexable
          ;_ (println "axis-ranges" axis-ranges)
          get-square (fn [top-left-x top-left-y size view] ;vec of vecs (with no nil), or nil if no such square (e.g. if a cell would be nil otherwise)
                       ;{:post [(or (nil? %) (square? %))]}
                       (let [axis-rng (axis-ranges size)
                             top-right-y+1 (+ top-left-y size)
                             result (vec (for    [x-within-square axis-rng]
                                           (subvec (view (+ x-within-square top-left-x)) top-left-y top-right-y+1)))]
                         ;(dbg-println "potential result:") (pprint-one-square result)
                         (if (empty? (for [x axis-rng
                                           y axis-rng
                                           :when (nil? ((result x) y))]
                                       :contains-nil))
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
                                          (map #(% col-index) square))]
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
                         prev-shifted-rows {} ;2-dimensional array {row-index-x {shift-of-that-row row-of-cells-with-nil-for-empty ..} ..}
                         shifts-leftover groups-of-shifts
                         res #{}]
                    (let [shifts (first shifts-leftover)
                          ; intermediate structure, seq. of seqs, where first cell is a seq. with 2-D indexes like for prev-shifted-rows, and second cell is a value (shifted row). This structure is easy to (apply assoc-in result-map ...)
                          shifted-rows-new-struc (for [x x-range
                                                       :let [shift (nth shifts x)
                                                             old (get-in prev-shifted-rows (list x shift))]
                                                       :when (nil? old)]
                                                   (let [row-orig (vecs-orig x)
                                                         row (vec (concat ;need a vector, so get-square can use (subvec..)
                                                                    (repeat shift nil)
                                                                    row-orig
                                                                    (repeat (- width shift (count row-orig)) nil)))] ;repeat accepts negative n => empty seq ()
                                                     (list (list x shift) row)))
                          shifted-rows-next (reduce
                                              (fn [res x-shift-row]
                                                (apply assoc-in res x-shift-row))
                                              prev-shifted-rows
                                              shifted-rows-new-struc) 
                          ;a 2-dimensional height X width vector of vectors, a representation of vecs-orig[] with applied shifts[]
                          view (vec (for [x x-range]
                                      ((shifted-rows-next x) (nth shifts x))))
                          ;specific per size, because latin squares of different size (generally) don't share parts
                          pack-shift-slice (fn [top-x size] (into {}
                                                              (map
                                                                (fn [x] [x (nth shifts x)])
                                                                (range top-x (+ top-x size)))))
                          res-in-groups-and-shifted-slices-new
                          (for  [size (range 2 (inc max-size))
                                 :let [top-left-y-range (axis-ranges (inc (- width size)))] ;excluding the last, since squares have size >=2
                                 top-left-x (axis-ranges (inc (- height size)))
                                 :let [shift-slice (#_dbgf pack-shift-slice top-left-x size)]
                                 :when (not (contains? prev-shift-slices shift-slice))]
                            
                            [(for [top-left-y top-left-y-range
                                   :let [;_ (println "shifts" shifts "top [" top-left-x top-left-y "size" size)
                                         square (#_dbgf get-square top-left-x top-left-y size view)]
                                   :when square] ;TODO 0. start with the rightmost column of the square (item set reused for -> #2) 1. add only if latin 2. then check +size-1 column to the right. Perform the whole check only if both columns have same item set.
                               ;                    \-> pass the reference column and its position, or the index set of the rest of the columns, as a parameter to latin?
                               square)
                             shift-slice])
                          res-new (apply concat
                                    (map first res-in-groups-and-shifted-slices-new))
                          res-next (into res res-new)
                          shift-slices-new (map second res-in-groups-and-shifted-slices-new)
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
          latin-squares (filter latin? squares)] ;filter latin? takes only 10% time
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
