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
(def dbg-indent-level (atom 0))
(defn dbg>> [] (swap! dbg-indent-level #(inc %)))
(defn dbg<< [] (swap! dbg-indent-level #(Math/max (dec %) 0))) ;TODO warn on negative?
(defn dbg-indentation [] (repeat @dbg-indent-level "  "))
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
  (dbg-println "Calling:" msg "with args: ")
  (dbg-pprint args)
  (dbg>>)
  (try
    (let [res (apply fun args)]
      (dbg<<)
      (dbg-println "Returning from:" msg "value:")
      (dbg-pprint res)
      res)
    (catch Throwable e
      (dbg<<)
      (dbg-print msg "Throwing from:" msg "throwable:" e)
      (throw e))))
    

(defmacro dbg [msgOrFun & others]
  (let [msg (if (string? msgOrFun)
              msgOrFun
              (str &form)) ;without (str) the macro would inject the user's code unqouted
        nilOrFun (if (string? msgOrFun)
                   nil
                   msgOrFun)
        fun (if nilOrFun
              nilOrFun
              (first others))
        args (if nilOrFun
               others
               (rest others))
        fun-expr (if (not (symbol? fun))
                   (if nilOrFun
                     (str (nth &form 1))
                     (str (nth &form 2))))
        fun-holder (gensym 'fun-holder)]
    (if true 
      (list 'do ; need (do) to honour any enclosing branches of e.g. (if)
        (if (not (symbol? fun))
          (list 'dbg-println "Evaluating a fn expression to call:" fun-expr "for:" msg))
        ;no need to pre-eval the function expression to call, because that is done as a part of calling dbg-call.
        (list 'let `[~fun-holder ~fun]
          (list 'dbg-println "Evaluating any args for:" msg)
          (seq (apply conj ['dbg-call msg fun-holder] args)))))))
  
;TODO alternative (list 'dbg-call msg nilOrFun others)

;replacement for skipping the dbg, but only for forms with a string message: 
;(defmacro dbg [& args] (rest &form))

;macroexpand wants a quoted expression:
;(macroexpand '(dbg ...))



























