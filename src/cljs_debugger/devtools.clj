(ns cljs-debugger.devtools
  (:require [clj-chrome-devtools.core :as chrome]
            [clj-chrome-devtools.events :as chrome-events]
            [clj-chrome-devtools.commands.page :as chrome-page]
            [clj-chrome-devtools.commands.runtime :as chrome-rt]
            [clj-chrome-devtools.commands.dom :as chrome-dom]
            [clj-chrome-devtools.commands.debugger :as chrome-dbg]
            [clj-chrome-devtools.commands.browser :as chrome-browser]
            [clojure.core.async :as a]
            [clojure.string :as s]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [cljs.source-map :as sm]
            [clojure.data.json :as json]))



;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


(defn setup-events! [c {:keys [on-break on-resume]}]
  (let [paused-chan (chrome-events/listen c :debugger :paused)
        resumed-chan (chrome-events/listen c :debugger :resumed)
        stop-chan (a/chan)
        stop-fn (fn []
                  (chrome-events/unlisten c :debugger :paused paused-chan)
                  (chrome-events/unlisten c :debugger :resumed resumed-chan)
                  ;; (a/close! paused-chan)
                  ;; (a/close! resumed-chan)
                  (a/close! stop-chan))]
    (a/go-loop []
      (a/alt!
        paused-chan ([val]
                     (when (fn? on-break)
                       (try
                         (on-break val)
                         (catch Exception e
                           (binding [*out* *err*] (println "Error in on-break handler:" e)))))
                     (recur))
        resumed-chan ([val]
                      (when (fn? on-resume)
                       (try
                         (on-resume val)
                         (catch Exception e
                           (binding [*out* *err*] (println "Error in on-resume handler:" e)))))
                      (recur))
        stop-chan (println "stopped")))
    {:stop stop-fn
     :resumed-chan resumed-chan
     :paused-chan paused-chan}))

;; (let [c (a/chan) close (a/chan)]
;;   (future (a/<!! (a/timeout 1000)) (a/close! close))
;;   (future (a/<!! (a/timeout 500)) (a/put! c 23))
;;   (a/go-loop [] (a/alt! c ([val] (println "og " val) (recur))
;;                         close (println "closed"))))


(defn js-file-of-frame
  "Takes the frame :url and converts it via cljs compiler env asset path and
  output-dir into a file on disk."
  [frame cenv]
  (let [cenv @cenv
        js-path (-> frame :url io/as-url .getPath)
        asset-path (-> cenv :options :asset-path)
        js-path (s/replace js-path (re-pattern (str "^/?" asset-path "/?")) "")
        output-dir (-> cenv :options :output-dir)
        js-file (io/file output-dir js-path)]
    js-file))


(defn source-map-of-js-file [frame-file]
  (let [lines (line-seq (io/reader frame-file))
        map-line (->> lines
                      reverse
                      (filter #(s/starts-with? % "//# sourceMappingURL="))
                      first)
        map-name (when map-line (last (s/split map-line #"=")))]
    (when map-name
      (io/file (.getParentFile frame-file) map-name))))

(defn source-map-lookup
  "`line-no` and `row-no` are zero-based."
  ;; [orig-cljs-file-name source-map-file line-no col-no]
  [mapping line-no col-no]
  (let [exact-row (get mapping line-no)
        exact-col (get exact-row col-no)

        exact-match (and exact-col (first exact-col))
        prev-in-row (and
                     (not exact-match)
                     (-> (for [col (range (dec col-no) -1 -1)
                               :let [entry (get exact-row col)]
                               :when entry]
                           entry)
                         first
                         first))
        prev-row (-> (for [row (range (dec line-no) -1 -1)
                           :let [entry (get mapping row)]
                           :when entry]
                       entry)
                     first vals last last)]
    (or exact-match prev-in-row prev-row)))

(defn source-map-lookup-reverse
  "`line-no` and `row-no` are one-based."
  [orig-cljs-file-name source-map-file line-no col-no]
  (let [orig-cljs-file-name-for-sm orig-cljs-file-name
        sm (sm/decode-reverse (json/read-json (slurp source-map-file) true))

        mapping (get sm orig-cljs-file-name-for-sm)]
    (source-map-lookup mapping line-no col-no)))

(defn connect
  ([]
   (connect nil))
  ([{:keys [host port] :or {host "localhost" port 9222} :as opts}]
   (let [c (chrome/connect host port)
         chrome-events (setup-events! c opts)]
     (chrome/set-current-connection! c)
     (chrome-dbg/enable c {})
     {:chrome-connection c
      :chrome-events chrome-events})))

(defn disconnect [{:keys [chrome-connection chrome-events] :as state}]
  (when-let [stop-events (some-> chrome-events :stop)] (stop-events))
  (some-> chrome-connection :ws-connection .close)
  (assoc state
         :chrome-connection nil
         :chrome-events nil))


(comment
  (chrome-browser/get-version c {})


  (def c (chrome/connect "localhost" 9222))
  (chrome/set-current-connection! c)
  (setup-events! c {})

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
