(ns cljs-debugger.scratch)

;; (let [c (a/chan) close (a/chan)]
;;   (future (a/<!! (a/timeout 1000)) (a/close! close))
;;   (future (a/<!! (a/timeout 500)) (a/put! c 23))
;;   (a/go-loop [] (a/alt! c ([val] (println "og " val) (recur))
;;                         close (println "closed"))))



(comment
  (chrome-browser/get-version c {})


  (def c (chrome/connect "localhost" 9222))
  (chrome/set-current-connection! c)
  (setup-events c {})

  (chrome-dbg/enable c {})
  (chrome-dbg/pause c {})

  (require 'clj-chrome-devtools.commands.target)
  (clj-chrome-devtools.commands.target/get-targets c {})
  (clj-chrome-devtools.commands.target/get-targets c {})
  (clj-chrome-devtools.commands.target/activate-target c {:target-id "B1AC9C50106F4DCB7D5EE52ED475A72D"})
  (clj-chrome-devtools.commands.target/set-auto-attach c {:auto-attach false :wait-for-debugger-on-start false})


  (chrome-page/navigate c {:url "http://webjure.org/"})
  (chrome-page/navigate c {:url "http://robert.kra.hn"})
  (chrome-page/navigate c {:url "http://localhost:8080"})
  (chrome-page/navigate c {:url "http://localhost:9500"})



  (def frame
    (let [evt (-> @event-state :events deref last)]
      (-> evt :params :call-frames first)))

  (def script-id (-> frame :location :script-id))
  (def loc (->> frame :location ((juxt :line-number :column-number)) (map inc)))
  (def call-frame-id (->> frame :call-frame-id))

  (def frame-file (js-file-of-frame frame cljs-debugger.build/cenv))
  (def js-file (cljs.closure/read-js js-file))
  (def js-file-source-map (source-map-of-js-file frame-file))




  (chrome-rt/get-properties c {:object-id (-> frame :scope-chain first :object :object-id)})

  (chrome-dbg/resume c {})

  (chrome-dbg/step-over c {})
  (chrome-dbg/step-into c {})
  (chrome-dbg/step-out c {})

  (chrome-dbg/evaluate-on-call-frame c {:expression "msg" :call-frame-id call-frame-id :silent true})
  (chrome-dbg/evaluate-on-call-frame c {:expression "this" :call-frame-id call-frame-id :silent true})


  (print (chrome-dbg/get-script-source c {:script-id script-id}))

  (def script-source (:script-source (chrome-dbg/get-script-source c {:script-id script-id})))

  123

  (def source-map-file (second (re-find #"//# sourceMappingURL=(.*)$" (first (reverse (s/split-lines script-source))))))


  (import '[com.google.debugging.sourcemap SourceMapConsumerV3])


  (def sm (SourceMapConsumerV3.))
  (def sm-file (io/file source-map-file))
  (.parse sm-consumer (slurp sm-file))
  (def mapping (.getMappingForLine sm-consumer line column))

  ;; https://webcache.googleusercontent.com/search?q=cache:OObj9op5xZEJ:https://clojureverse.org/t/server-side-decoding-of-javascriptsourcemaps/1591+&cd=11&hl=en&ct=clnk&gl=de&lr=lang_de%7Clang_en&client=ubuntu
  (defn lookup [source-map-file line column]
    (let [sm-consumer (SourceMapConsumerV3.)
          sm-file (io/file source-map-file)]
      (.parse sm-consumer (slurp sm-file))
      (when-let [mapping (.getMappingForLine sm-consumer line column)]
        {:original (.getOriginalFile mapping)
         :line (.getLineNumber mapping)
         :column (.getColumnPosition mapping)}
        )))

  (lookup "./resources/public/js/cljs_debugger/browser.js.map" 6 65)
  (lookup "./resources/public/js/cljs_debugger/browser.js.map" 9 24)
  (apply lookup "./resources/public/js/cljs_debugger/browser.js.map" loc)
  (apply lookup js-file-source-map loc)
  ;; => {:original "core.cljs", :line 3767, :column 5}

  (lookup "./resources/public/js/cljs_debugger/browser.js.map" 9 1)

  (slurp (io/file "./resources/public/js/cljs_debugger/browser.js.map"))




  ;; source map via cljs.source-map
  (require '[clojure.data.json :as json])
  (into (sorted-map-by <)
        (cljs.source-map/decode
         (json/read-str
          (slurp "resources/public/js/cljs/core.js.map")
          :key-fn keyword)))
  )
