(require 'clojure.pprint)
 
;TODO How to ensure the file is loaded as the first (or before a set of files), so that it re-defines 'fn' macro for them?
; -> future: redefine defn, fn

;BIG TODO: wrap everythin in with-out-str somehow, so it indents user's calls to print.
;TODO (time) - optional?
(defn dbg-show-function [value]
  (def ^:dynamic dbg-show-function-forms value))
(dbg-show-function false)


(def ^:dynamic dbg-indent-level 0)
(defn dbg-indent-plus [] (def ^:dynamic dbg-indent-level (inc dbg-indent-level)))
(defn dbg-indent-minus [] (def ^:dynamic dbg-indent-level (Math/max (dec dbg-indent-level) 0))) ;TODO warn on negative, but prevent further dbg-unindent reporting
(defn dbg-indentation [] (apply str (repeat dbg-indent-level "  ")))

;By default we don't indent the first line, so it can be appended to an existing content.
(defn dbg-indent
  ([content] (dbg-indent content false))
  ([content indentFirstLine]
   (let [indentedOtherLines (clojure.string/replace content "\n" (str "\n" (dbg-indentation)))]
     (if indentFirstLine
       (str (dbg-indentation) indentedOtherLines)
       indentedOtherLines))))

;alternatively: (binding [*out* ...] (callback...)) or (def *out* ....)
(defn dbg-print [& args]
  (print (dbg-indent
           (reduce
             #(if (= % "")
                %2
                (str % \space %2))
             "" args)
           true)))
(defn dbg-println [& args]
  (apply dbg-print args)
  (println)) ;Don't append "\n" to args of dbg-print, because it calls dbg-indent which removes a trailing newline.

; A helper to capture output of clojure.pprint/pprint. Remove an extra newline at the end.
(defn pretty [obj]
  ;Good that Clojure regex doesn't use Java Regex MULTILINE-like "m?" by default, because we want to exclude the last line only
  (clojure.string/replace (with-out-str (clojure.pprint/pprint obj)) #"\n$" ""))

;TODO Print long or multi-line obj starting on a separate line
;Print prefix, a space, and pretified obj.
; Unlike clojure.pprint/pprint, this does *not* append a newline.
(defn dbg-pprint-last [prefix obj]
  (dbg-print prefix (pretty obj)))

;---- For (dbg...), which works with either functions, macros or special forms
(defn dbg-call-before [msg & args]
  ; Here and in dbg-*: Don't add colon : to printout, because it doesn't look good if msg is a keyword.
  (let [call-msg (str "Call " msg)]
    (if (seq args)
      (dbg-pprint-last (str call-msg " with") args)
      (dbg-print call-msg)))
  (println)
  (dbg-indent-plus))

(defn dbg-call-after [msg res]
  (dbg-indent-minus)
  (dbg-pprint-last (str "From " msg " return") res)
  (println))

(defn dbg-call-throw [msg e]
  (dbg-indent-minus)
  (dbg-println msg "Throw" msg "throwable:" e)
  (throw e))
  
;This is a macro and not a function, so we can use `dbg` with other macros/special forms.
;Otherwise users may need to wrap code in #(...) or (fn [] ....). That not only adds a set of parenthesis.
;It also upsets any (recur...) from inner code (until https://dev.clojure.org/jira/browse/CLJ-2235).
(defmacro dbg-call [msg & form]
  (list 'do
    (concat `(dbg-call-before ~msg) args)
    (list 'try
       (list
          'let ['res (concat (list fun) args)]
          (list 'dbg-call-after msg 'res)
          'res)
       (list
          'catch 'Throwable 'e
          (list 'dbg-call-throw msg 'e)))))

; an alternative to dbg-call, but it only works with functions, not with macros/special forms
(defn dbg-call-f [msg fun & args]
  ; Here and in dbg macro: Don't use colon : in printout, because it doesn't look good if msg is a keyword.
  (let [call-msg (str "Call " msg)]
    (if (seq args)
      (dbg-pprint-last (str call-msg " with") args)
      (dbg-print call-msg)))
  (println)
  (dbg-indent-plus)
  (try
    (let [res (apply fun args)]
      (dbg-indent-minus)
      (dbg-pprint-last (str "From " msg " return") res)
      (println)
      res)
    (catch Throwable e
      (dbg-indent-minus)
      (dbg-println msg "Throw" msg "throwable:" e)
      (throw e))))

; If we need to treat a string into a symbol literal-compatible string. See https://clojure.org/reference/reader#_symbols

;https://clojure.org/guides/weird_characters
; - every time a particular x# is used within a single syntax quote, the _same_ generated name will be used.

;Invoke either
; - without a message: (dbg function args...), (dbg (function-expr) args...)
; - with a message as a string literal:
; - with a message as a keyword literal - the
(def dbg-snapshot-prefix "dbg-snapshot")

;TODO pprint of function expression < https://clojuredocs.org/clojure.pprint/pprint#example-5b950e6ce4b00ac801ed9e8a
; -- (clojure.pprint/with-pprint-dispatch clojure.pprint/code-dispatch (clojure.pprint/pprint (clojure.edn/read-string "code-as-string-here") ))

; Insert `dbg "description"` before function calls, like
; `(dbg "+ on numbers" + 1 2)`
; Beware of lazy sequences when tracing errors: for example, (for) creates a lazy sequence, hence callbacks will be delayed

; Insert 'dbg' in front of most calls, except for:
; - special forms and macros. Wrap them in #(...) of (fn [] ...)
; - keyword literal serving as an accessor function, for example (:i {:i 1}). For them, either
; --- insert a string literal message (but not another keyword literal): (dbg ":i from a map" :i {:i 1}), or
; --- insert :_, followed by a keyword literal (to set a scope/reference for inner (dbg) calls) or :_. For example
;     (dbg :_ :i-from-a-map :i {:i 1}) or (dbg :_ :_ :i {:i 1})
; No need to insert anything in front of a symbol literal serving as an accessor function.
; For example ('i {'i 1}) => (dbg 'i {'i 1}). (Plus, any function names are also symbols, and we want them to work intuitively.)
(defmacro dbgf [msgOrFun & others]
  (let [firstKeyword (if (keyword? msgOrFun) msgOrFun)
        secondKeyword (if (and
                               firstKeyword
                               (keyword? (first others)))
                        (first others))
        msgAsGiven (or
                       (and (string? msgOrFun) msgOrFun)
                       (and (not= firstKeyword :_) (not secondKeyword) firstKeyword)
                       (and (not= secondKeyword :_) secondKeyword))
        ;firstIsNotFunction may be true even though msgAsGiven is nil, if keyword(s) are :_
        ;Can't do negative check for firstIsNotFunction, because a function may be represented by a symbol or a list (to evaluate)
        firstIsNotFunction (or (string? msgOrFun) (keyword? msgOrFun))
        ; "logical" (with a variable position among parameters):
        msg (or msgAsGiven (str &form)) ;without (str) the macro would inject the user's code unqouted
        fun (if (not msgAsGiven)
              msgOrFun
              (if secondKeyword
                (second others)
                (first others)))
        args (if (and
                      (not (string? msgOrFun))
                      (not (keyword? msgOrFun)))
               others
               (if secondKeyword
                 (drop 2 others)
                 (rest others)))
        fun-expr (if (and
                          (not (symbol? fun))
                          (not (keyword? fun))) ;a keyword if accessing a map entry
                   (if firstIsNotFunction
                     (if secondKeyword
                       (str (nth &form 3))
                       (str (nth &form 2)))
                     (str (nth &form 1))))
        fun-holder (gensym 'fun-holder)
        scopeBackReferenceKeyword (if (and firstKeyword (not= firstKeyword :_) secondKeyword)
                                    firstKeyword)
        scopeForwardDefinitionKeyword (if firstKeyword
                                        (if secondKeyword
                                          (if (not= secondKeyword :_) secondKeyword)
                                          (if (not= firstKeyword  :_) firstKeyword)))]
    (let [declare-binding (list 'binding ['dbg-indent-level
                                          (list 'inc (symbol (str dbg-snapshot-prefix scopeBackReferenceKeyword)))])
          declare-let (list 'let [(symbol (str dbg-snapshot-prefix scopeForwardDefinitionKeyword)) 'dbg-indent-level])
          execute (concat
                    (if (and
                             (not (symbol? fun))
                             dbg-show-function-forms)
                      (list 'dbg-println "Fn for" msg "<-" fun-expr))
                    ;no need to pre-eval the function expression to call, because that is done as a part of calling dbg-callf.
                    (list
                      (list 'let `[~fun-holder ~fun] ;let allows us to separate any logs of the function-generating expression from the targt function call.
                        (if (seq args)
                          (list 'dbg-println "Args for" msg))
                        (seq (apply conj ['dbg-callf msg fun-holder] args)))))]
      (concat
        (if scopeBackReferenceKeyword 
          (concat
            declare-binding
            (if scopeForwardDefinitionKeyword
              (list
                (concat declare-let execute))
              execute))
          
          (if scopeForwardDefinitionKeyword
            (concat declare-let execute)
            (concat '(do) execute)))))))

; Create a scope that you can refer to from dbg or dbgf. This works with either a function, a macro or a special form.
(defmacro dbg-scope [scope-keyword invoke & args]
  (assert (keyword? scope-keyword) "scope-keyword must be a keyword")
  (list
    'let [(symbol (str dbg-snapshot-prefix scope-keyword)) 'dbg-indent-level]
    (cons invoke args)))
          
  
; TODO dbg>> for cross-thread keyword references
; TODO dbg-cfg macro

; Problem with code generated by a macro?
; 1. Run macro-expand, macroexpand-1 macroexpand-all (with the code quoted)
; 2. (clojure.pprint/with-pprint-dispatch clojure.pprint/code-dispatch (clojure.pprint/pprint 'generated-code-here ))

;replacement for skipping the dbg, but only for forms with a string message: 
;(defmacro dbg [one & others] (rest &form))

(if false
   (dbgf :out (fn[]
                (dbgf :out :in (fn []
                                 #_(println "in" dbg-indent-level)
                                 (dbgf :in :innermost #(println "innermost" dbg-indent-level)))))))
(if false
   (dbg :out (fn[]
               (dbg :out :in (fn []
                               #_(println "in" dbg-indent-level)
                               (dbg :in :innermost #(println "innermost" dbg-indent-level)))))))

(if false
  (dbg :out (fn[])
    (println "out" dbg-indent-level)
    (dbg :out :in #(println "in" dbg-indent-level))))
(if false
  (defn arity-test [[one two]])
  (if false (arity-test 0))) ;function arity checks are only done when evaluating
;but symbols are checked when compiling:
#_(if false (missing-function))
#_(if false (#(missing-function)))
(if false ((fn [par]) #_missing_par))


















