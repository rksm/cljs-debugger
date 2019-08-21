(ns cljs-debugger.core
  (:require [clojure.data :as data]))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defn test
  [a b]
  (+ a b (quot a b)))

(test 4 3)


(comment

  (def ast (cljs.analyzer.api/analyze aenv '(def js/x 23)))
  (def ast (cljs.analyzer.api/analyze aenv '(defn test [x] (+ x 2))))
  (def ast (cljs.analyzer.api/analyze aenv '(set! js/window.x 23)))
  (data/diff aenv (:env ast))

  {:a aenv
   :b(:env ast)}


  (cljs.compiler.api/emit ast)



  (require '[cider.nrepl.middleware.util.instrument :as ins])
  (require '[cider.nrepl.middleware.debug :as d])


  cider.nrepl.middleware.debug

  (def instrument #'cider.nrepl.middleware.util.instrument/instrument)
  (def with-break #'cider.nrepl.middleware.util.instrument/with-break)


  (instrument (with-break (with-meta '(fooo (bar 1 2) 3)
                            {:cider.nrepl.middleware.util.instrument/breakfunction 'BREAK
                             :cider.nrepl.middleware.util.instrument/extras 23})))


  (let [code "(defn x [] (foo 23 #break (bar 22)))"
        has-debug?  (volatile! false)
        fake-reader (fn [x] (vreset! has-debug? true) x)]
    (binding [*ns* (find-ns (symbol "user"))
              *data-readers* (->> (repeat fake-reader)
                                  (interleave '[dbg break light])
                                  (apply assoc *data-readers*))]
      (try
        ;; new-line in REPL always throws; skip for debug convenience
        (when (> (count code) 3)
          (read-string {:read-cond :allow} code))
        (catch Exception _e)))
    @has-debug?)


  (meta (cider.nrepl.middleware.util.instrument/instrument-tagged-code '(bar #break (fooo))))

  (defn test [x] (+ x 2))

  (cider.nrepl.middleware.debug/instrument-and-eval '(do #break (test 3)))

  (clojure.walk/postwalk (juxt identity meta) (cider.nrepl.middleware.debug/instrument-and-eval '(do #dbg (test 3))))
  (clojure.walk/postwalk (juxt identity meta) (d/debug-reader '(do #dbg (test 23))))


  (clojure.walk/postwalk (juxt identity meta) (cider.nrepl.middleware.util.instrument/tag-form-recursively '(do #dbg (test 3)) 'x))


  (cider.nrepl.middleware.util.instrument/breakpoint-tester '(do #dbg (test 3)))



  (macroexpand-1 (d/breakpoint-if-interesting (inc 10) {:coor [6]} ~'(inc 10)))
  (macroexpand
   `(d/with-initial-debug-bindings
      (d/breakpoint-if-interesting (inc 10) {:coor [6]} ~'(inc 10))))

  (read-string "(do #dbg (test 23))")

  (d/debug-reader '(do #dbg (test 23)))
  (ins/instrument-tagged-code (d/debug-reader '(do #dbg (test 23))))

  (d/with-initial-debug-bindings
    (d/breakpoint-if-interesting (inc 10) {:coor [6]} ~'(inc 10)))
  )
