(ns cljs-debugger.macros
  (:require [clojure.pprint :refer [cl-format]]))



(comment

 (defmacro xxx
   ""
   [val]
   ;; (def e &env)
   `(do
      ~@(seq (for [[sym] &env] `(cl-format true "~a: ~s~%" '~sym ~sym)))
      ~val))



 ;; (let [foo 23] (prn (xxx)))

 ;; (defn foo [x] (xxx (+ x 3)))
 (defn foo [] (xxx 3))
 (foo)
 )
