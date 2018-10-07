(require 'clojure.pprint)

(defmacro practice-small-debug-macro [msg body]
 `(let [val# ~body]
   (println "DEBUG (MSG): " ~msg)
   (println "DEBUG (RET): " val#)
   val#))

(defmacro practice-use-form [s]
  (let [f (str &form)]
    `(let [s# ~s] (str s# ~f s#))))
; insert `trace "description"` before function calls, like
; (trace "+ on numbers" + 1 2)
; Unfortunately, that disables compile time validation of number of parameters, so if functions/calls change,
; to validate them remove `trace "message", compile, re-add trace "message" back if need be.
; Beware when tracing errors: (for) creates a lazy sequence, hence callbacks will be delayed
(defn trace [msg f & args]
  ((print "Calling" msg "with ")   
   (clojure.pprint/pprint args)
   (try
     (let [res (apply f args)]
       (print msg "returning ")
       (clojure.pprint/pprint res)
       res)
     (catch Throwable e
       (print msg "threw" e)
       (throw e)))))
 
;TODO How to ensure the file is loaded as the first (or before a set of files), so that it re-defines 'fn' macro for them?

;BIG TODO: wrap everythin in with-out-str somehow, so it indents user's calls to print.
;TODO (time) - optional?
(defn dbg-show-function [value]
  (def ^:dynamic dbg-show-function-forms value))
(dbg-show-function false)


(def ^:dynamic dbg-indent-level 0)
(defn dbg-indent [] (def ^:dynamic dbg-indent-level (inc dbg-indent-level)))
(defn dbg-unindent [] (def ^:dynamic dbg-indent-level (Math/max (dec dbg-indent-level) 0))) ;TODO warn on negative, but prevent further dbg-unindent reporting
(defn dbg-indentation [] (repeat dbg-indent-level "  "))
(defn dbg-format [content]
  (clojure.string/replace content #"\r?\n" (str (newline) (dbg-indentation)))) ;not using (newline) for the pattern, so that hard-coded new line character(s) work cross-platform.
;alternatively: (binding [*out* ...] (callback...)) or (def *out* ....)
(defn dbg-print [& args]
  (apply print (map dbg-format args)))
(defn dbg-println [& args]
  (apply dbg-print args)
  (println))
; clojure.pprint/pprint appends a line after
(defn dbg-pprint [obj]
  (let [unindented (with-out-str (clojure.pprint/pprint obj))
        indented (dbg-format unindented)]
    (print indented)))

(defn dbg-call [msg fun & args]
  ; Here and in dbg macro: Don't use colon : in printout, because it doesn't look good if msg is a keyword.
  (dbg-print "Call" msg)
  (if (seq args)
    (do (println " with ")
      (dbg-pprint args))
    (println))
  (dbg-indent)
  (try
    (let [res (apply fun args)]
      (dbg-unindent)
      (dbg-println "Return" msg "value:")
      (dbg-pprint res)
      res)
    (catch Throwable e
      (dbg-unindent)
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

; Insert 'dbg' in front of most calls, except for:
; - special forms and macros. Wrap them in #(...) of (fn [] ...)
; - keyword literal serving as an accessor function, for example (:i {:i 1}). For them, either
; --- insert a string literal message (but not another keyword literal): (dbg ":i from a map" :i {:i 1}), or
; --- insert :_, followed by a keyword literal (to set a scope/reference for inner (dbg) calls) or :_. For example
;     (dbg :_ :i-from-a-map :i {:i 1}) or (dbg :_ :_ :i {:i 1})
; It works with a symbol literal serving as an accessor function, for example ('i {'i 1}) => (dbg 'i {'i 1}).
; (No need to insert anything in front of 'i).
(defmacro dbg [msgOrFun & others]
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
                    ;no need to pre-eval the function expression to call, because that is done as a part of calling dbg-call.
                    (list
                      (list 'let `[~fun-holder ~fun] ;let allows us to separate any logs of the function-generating expression from the targt function call.
                        (if (seq args)
                          (list 'dbg-println "Args for" msg))
                        (seq (apply conj ['dbg-call msg fun-holder] args)))))]
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

; TODO dbg>> for cross-thread keyword references
; TODO dbg-cfg macro

; Problem with code generated by a macro?
; 1. Run macro-expand, macroexpand-1 macroexpand-all (with the code quoted)
; 2. (clojure.pprint/with-pprint-dispatch clojure.pprint/code-dispatch (clojure.pprint/pprint 'generated-code-here ))

;replacement for skipping the dbg, but only for forms with a string message: 
;(defmacro dbg [& args] (rest &form))

(defn inner [arg] 1)
(defn outer []
  (dbg :w->o + 1 (dbg :w->o :>in inner 4)))























