;http://www.4clojure.com/problem/127 Triangles
;the comments in examples suggest a transformation, but they're only to illustrate only!
;the return value is the size of the area, or nil.
;How do it? Pick a corner point. Take one of 8 directions - one side of triangle.
;The other side of triangle has two options: the next (neighbouring) direction (45 degrees)
;or the second next direction (90 degrees).
(def triangle
  (fn [decimals]
    (let [mx (for [line decimals
                   digit (seq (Integer/toString line 2))]
               ({\0 false \1 true} digit))
          dirs #_from-top-left-clockwise [[-1 -1] [-1 0] [-1 1] [0 1] [1 1] [1 0] [1 -1] [0 -1]]
          numbered-dir #_rotate-index-overflow (fn [num] (dirs (rem num 8)))
          line (fn [])
          right-of (fn []) ;<<<<
          left-of (fn [])]
      1)))

