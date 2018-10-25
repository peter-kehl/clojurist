(use 'clojure.set)

;http://www.4clojure.com/problem/152 LAtin Squares
(def latin
  (fn [vecs-orig]
    (let [;MIN-OPTIMISED-SIZE 2 ;Increasing to 3 slowed down the last (biggest) test at http://www.4clojure.com/problem/152 from 206ms to 300ms! 
          ;MIN-VEC-OPS-SIZE 4
          height (count vecs-orig)
          max-x (dec height)
          width (apply max (map count vecs-orig))
          max-y (dec width)
          max-size (min width height)
          MAX-OPTIMISED-SIZE (- max-size 1);Definitely don't optimise squares with size equal to max-size, because such squares fit exactly in one
          ;(or even both) dimensions of vecs-orig[][], hence even without optimisation they're processed only once.
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
          get-square (fn [top-left-x top-left-y shifts size] ;vec of vecs (with no nil), or nil if no such square (e.g. if a cell would be nil otherwise)
                       ;{:post [(or (nil? %) (square? %))]}
                       (let [;use-vector-ops (<= MIN-VEC-OPS-SIZE size)
                             ; *-orig coordinates are within the whole matrix vecs-orig AND after applying any shifts
                             result-list (for [x-orig (range top-left-x (+ top-left-x size))
                                               :let [row-orig (vecs-orig x-orig)
                                                     top-left-y-orig (- top-left-y #_shift==> (shifts x-orig))]
                                               :while (<= 0 top-left-y-orig)
                                               :let [top-left-y-orig+size (+ top-left-y-orig size)]
                                               :while (<= top-left-y-orig+size (count row-orig))]
                                           (subvec row-orig top-left-y-orig top-left-y-orig+size))]
                         ;(dbg-println "potential result:") (pprint-one-square result)
                         (if (= (count result-list) size)
                           (vec result-list) 
                           nil)))
          ; a list of vectors, each cell containing a shift (0 or higher) of its respective vector (row) in vecs-orig[].
          ; They are vectors rather than seq, so that pack-shift-slice can use (subvec ...) on them.
          groups-of-shifts (letfn [(sub-shifts-since-level [level]
                                     (let [results-below (if (< level max-x) #_alternativ-to-memoize
                                                           (sub-shifts-since-level (inc level))
                                                           :unused)]
                                       (apply concat
                                         (for [shift (axis-ranges (inc (- width (count (vecs-orig level)))))]
                                           (if (= level max-x)
                                             [[shift]]
                                             (map
                                               #(conj % shift)
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
          ;those shifts that were processed already as a subset of previous shifts. (This optimisation is partial, as two different shift groups may shift
          ;two or more neighbouring rows by the same shift.)
          ;prev-shift-slices - a structure containing index-of-row & shift-for-that-row for all relevant shifted rows -
          ;for two or more consecutive rows. (The actual structure doesn't matter, as far as it's comparable and storable in a hash-set.
          ;The simpler the better. Simplifying it from a map {} to a seq. of seq. saves 10% of time.)
          ;If vecs-orig[] has more than two rows, then any shift generates multiple entries in prev-shift-slices #{},
          ;to cover all combinations of two or more consecutive rows.
          ;Because we're caching/skipping based on shifts, each (loop) iteration processes one shift completely.
          squares (loop [prev-shift-slices #{}
                         shifts-leftover groups-of-shifts
                         res #{}]
                    (let [shifts (first shifts-leftover)
                          res-in-groups-and-shifted-slices-new
                          (for  [size (range 2 (inc max-size))
                                 :let [pack-shift-slice (if (<= size MAX-OPTIMISED-SIZE)
                                                          (if (= size 2)
                                                            (fn [top-x] ;optimised version
                                                              (list top-x (shifts top-x) (shifts (inc top-x))))
                                                            ;result is specific per size, because latin squares of different size (usually) don't share parts
                                                            (fn [top-x] 
                                                              (conj
                                                                (subvec shifts top-x (+ top-x size))
                                                                top-x))))
                                                                  
                                       top-left-y-range (axis-ranges (inc (- width size)))] ;excluding the last, since squares have size >=2
                                 top-left-x (axis-ranges (inc (- height size)))
                                 :let [shift-slice (if ;and (<= MIN-OPTIMISED-SIZE size
                                                     (<= size MAX-OPTIMISED-SIZE)
                                                     (pack-shift-slice top-left-x))] ;Optimisation only for squares of size >=MIN-OPTIMISED-SIZE
                                 :when (or ;(< size MIN-OPTIMISED-SIZE)
                                           (< MAX-OPTIMISED-SIZE size)
                                           (not (contains? prev-shift-slices shift-slice)))]
                            [(for [top-left-y top-left-y-range
                                   :let [;_ (println "shifts" shifts "top [" top-left-x top-left-y "size" size)
                                         square (#_dbgf get-square top-left-x top-left-y shifts size)]
                                   :when square] ;TODO 0. start with the rightmost column of the square (item set reused for -> #2) 1. add only if latin 2. then check +size-1 column to the right. Perform the whole check only if both columns have same item set.
                               ;                    \-> pass the reference column and its position, or the index set of the rest of the columns, as a parameter to latin?
                               square)
                             shift-slice])
                          res-new (apply concat
                                    (map first res-in-groups-and-shifted-slices-new))
                          res-next (into res res-new)
                          shift-slices-new (filter identity #_exclude-nil-to-speedup-the-below-into
                                             (map second res-in-groups-and-shifted-slices-new)) ;for size<MIN-OPTIMISED-SIZE or MAX-OPTIMISED-SIZE<size this is (list nil) - still OK
                          shift-slices-next (into prev-shift-slices shift-slices-new)
                          shifts-leftover-next (next shifts-leftover)]
                      (if shifts-leftover-next
                        (recur
                          shift-slices-next
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
