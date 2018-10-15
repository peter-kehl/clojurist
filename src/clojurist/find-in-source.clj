(require 'clojure.repl)
; both clojure.repl/dir and clojure.repl/source print out, but return nil.
; However, clojure.repl/source-fn *returns* the text, instead of printing out!
(if true
  (filter
    #_(fn [_] true)
    (comp not (partial re-find #"\([ \t]*conj"))
    (filter
      (comp not nil?) ;some source(s) were nil!
      (map
        (comp clojure.repl/source-fn symbol)
        (filter
          (comp not (partial = "conj"))
          (clojure.string/split-lines
            (with-out-str (clojure.repl/dir clojure.core))))))))

