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
 

;BIG TODO: wrap everythin in with-out-str somehow, so it indents user's calls to print.
;TODO (time) - optional?
(defn dbg-show-function [value]
  (def ^:dynamic dbg-show-function-forms value))
(dbg-show-function false)

(def ^:dynamic dbg-indent-level 0)
(defn dbg-indent [] (def ^:dynamic dbg-indent-level (inc dbg-indent-level)))
(defn dbg-unindent [] (def ^:dynamic dbg-indent-level (Math/max (dec dbg-indent-level) 0))) ;TODO warn on negative, but prevent further dbg-unindent reporting
(defn dbg-indentation [] (repeat dbg-indent-level "  "))
(defn dbg-format [str]
  (clojure.string/replace str "\n"#"\r?\n" (str (newline) (dbg-indentation)))) ;not using (newline) for the pattern, so that hard-coded new line character(s) work cross-platform.
;alternatively: (binding [*out* ...] (callback...)) or (def *out* ....)
(defn dbg-print [& args]
  (apply print (map dbg-format args)))
(defn dbg-println [& args]
  (apply dbg-print args)
  (println))
(defn dbg-pprint [obj]
  (let [unindented (with-out-str (clojure.pprint/pprint obj))
        indented (dbg-format unindented)]
    (print indented)))

(defn dbg-call [msg fun & args]
  (dbg-print "Call:" msg)
  (if (seq args)
    (do (println " with: ")
        (dbg-pprint args))
    (println))
  (dbg>>)
  (try
    (let [res (apply fun args)]
      (dbg<<)
      (dbg-println "Return:" msg "value:")
      (dbg-pprint res)
      res)
    (catch Throwable e
      (dbg<<)
      (dbg-println msg "Throw:" msg "throwable:" e)
      (throw e))))

; If we need to treat a string into a symbol literal-compatible string. See https://clojure.org/reference/reader#_symbols

;https://clojure.org/guides/weird_characters
; - every time a particular x# is used within a single syntax quote, the _same_ generated name will be used.

;Invoke either
; - without a message: (dbg function args...), (dbg (function-expr) args...)
; - with a message as a string literal:
; - with a message as a keyword literal - the
;TODO snapshot indent level when called

(defmacro dbg [msgOrFun & others]
  (let [msgIsGiven (or (string? msgOrFun) (keyword? msgOrFun))
        msg (if msgIsGiven 
              msgOrFun
              (str &form)) ;without (str) the macro would inject the user's code unqouted
        msgKeyword (if (keyword? msgOrFun) msgOrFun)
        firstFun (if msgIsGiven
                   nil
                   msgOrFun)
        secondKeyword (if (and
                               msgKeyword
                               (keyword (first others)))
                        (first others))
        fun (if firstFun
              firstFun
              (if secondKeyword
                (first (first others))
                (first others)))
        args (if firstFun
               others
               (if secondKeyword
                 (rest (rest others))
                 (rest others)))
        fun-expr (if (not (symbol? fun))
                   ;the following was (before introducing secondKeyword): (if firstFun (str (nth &form 1)) (str (nth &form 2)))
                   (if msgIsGiven
                     (if secondKeyword
                       (str (nth &form 3))
                       (str (nth &form 2)))
                     (str (nth &form 1))))
        fun-holder (gensym 'fun-holder)]
    (list 'do ; need (do) to honour any enclosing branches of e.g. (if)
      (if (not (symbol? fun))
        (list 'if dbg-show-function-forms 
          (list 'dbg-println "Fn for:" msg "<-" fun-expr)))
      ;no need to pre-eval the function expression to call, because that is done as a part of calling dbg-call.
      (list 'let `[~fun-holder ~fun] ;let allows us to separate any logs of the function-generating expression from the targt function call.
        (if (seq args)
          (list 'dbg-println "Args for:" msg))
        (seq (apply conj ['dbg-call msg fun-holder] args))))))
  
;TODO alternative (list 'dbg-call msg nilOrFun others)

;replacement for skipping the dbg, but only for forms with a string message: 
;(defmacro dbg [& args] (rest &form))

;macroexpand wants a quoted expression:
;(macroexpand '(dbg ...))



























