(ns cljs-debugger.build
  (:require cljs.analyzer.api
            cljs.compiler.api
            cljs.compiler
            [cljs.build.api :as b]))


(def opts {:output-to "resources/public/js/main.js"
           :output-dir "resources/public/js"
           :optimizations :none
           :main 'cljs-debugger.browser
           :verbose true
           :asset-path "js"

           :closure-warnings
           {:check-types :error ;; << ADD THIS
            :undefined-names :off
            :externs-validation :off
            :missing-properties :off}})

(def cenv (let [cenv (cljs.analyzer.api/empty-state)]
            (swap! cenv assoc :options opts)
            cenv))

(def aenv (cljs.analyzer.api/empty-env))

;; (cljs.compiler.api/compile-root cenv "./src" "./out" {})

;; (binding [cljs.env/*compiler* cenv]
;;   (cljs.compiler/with-core-cljs)
;;   (cljs.compiler.api/compile-file
;;    cenv "./src/cljs_debugger/browser.cljs" "./out/main.js" opts))




(b/build "src" opts cenv)


(comment
  
  (cljs.compiler.api/cljs-files-in (clojure.java.io/file "."))

  (binding [cljs.env/*compiler* cenv]


 (cljs.closure/js-source-file nil (io/file "resources/public/js/cljs/core.js"))
 (cljs.closure/read-js (io/file "resources/public/js/cljs/core.js"))

 (cljs.build.api/inputs)
 (cljs.build.api/target-file-for-cljs-ns 'cljs.core (:output-dir opts)))
 (cljs.build.api/ns->location 'cljs.core cenv))
