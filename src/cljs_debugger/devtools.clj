(ns cljs-debugger.devtools
  (:refer-clojure :exclude [next])
  (:require [clj-chrome-devtools.commands.debugger :as chrome-dbg]
            [clj-chrome-devtools.core :as chrome]
            [clj-chrome-devtools.events :as chrome-events]
            [cljs-debugger.reading :as r]
            [cljs.analyzer.api :as ana-api]
            [cljs.source-map :as sm]
            [clojure.core.async :as a]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as s]))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

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

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defn find-more-precise-loc [js-file from-line name]
  (let [rdr (io/reader js-file)]
    (doall (repeatedly from-line #(.readLine rdr)))
    (loop [line from-line]
      (let [line-string (.readLine rdr)
            col (and line-string (s/index-of line name))]
        (cond
          (nil? line-string) nil
          col {:line line :col col}
          :else (recur (inc line)))))))

(defn find-more-precise-loc-logged [js-file from-line name]
  (let [r (find-more-precise-loc js-file from-line name)]
    (println from-line r)
    r))


(defn analyze-frame
  "Starting from a CDP frame from a debugger break location, tries to find a cljs
  top-level form that represents the code at the break location. If found, the
  returned form will contain meta data from indexed reading as well as the
  source location in the coor vector form used by the cider debugger."
  [frame cenv]
  (let [js-file (js-file-of-frame frame cenv)
        loc (->> frame :location ((juxt :line-number :column-number)))

        sourcemap-file (source-map-of-js-file js-file)
        closure-js-file (-> @cenv :cljs.closure/compiled-cljs (get (.getCanonicalPath js-file)))
        ;; source-map-cljs-name (.getName (io/file (:source-url closure-js-file)))
        ;; script-id (-> frame :location :script-id)
        ;; call-frame-id (->> frame :call-frame-id)

        sm (json/read-json (slurp sourcemap-file) true)
        {:keys [line col name] :as cljs-loc} (apply source-map-lookup (sm/decode sm) loc)
        _ (println name [line col])
        [line col] (if-let [{:keys [line col]} (and name (find-more-precise-loc-logged (:url closure-js-file) line name))]
                     [line col]
                     [line col])
        _ (println "vs" [line col])
        ns-ast (ana-api/find-ns cenv (:ns closure-js-file))
        ;; NOTE! defs in cljs have one-indexed lines!
        defs (sort-by (comp - :line) (vals (:defs ns-ast)))
        top-level-def (let [cljs-line (inc line)]
                        (first
                         (for [def defs
                               :when (<= (:line def) cljs-line)]
                           def)))
        top-level-form (r/read-top-level-form-of-def-from-file
                        top-level-def
                        (:source-url closure-js-file))
        form (or (r/find-nested-form-closest (inc line) (inc col) top-level-form)
                 top-level-form)]
    ;; (sc.api/spy)
    {:frame frame
     :top-level-form top-level-form
     :form form
     :closure-js-file closure-js-file
     :cljs-loc cljs-loc}))

(comment

  (import '[com.google.debugging.sourcemap SourceMapConsumerV3])

  (defn lookup [source-map-file line column]
    (let [sm-consumer (SourceMapConsumerV3.)
          sm-file (io/file source-map-file)]
      (.parse sm-consumer (slurp sm-file))
      (when-let [mapping (.getMappingForLine sm-consumer line column)]
        {:original (.getOriginalFile mapping)
         :line (.getLineNumber mapping)
         :column (.getColumnPosition mapping)}
        )))

  (apply lookup sourcemap-file loc)


  (sc.api/defsc 6)
  cljs-loc
  loc
  (sc.api/letsc 5 [loc])
  (sc.api/letsc 6 [loc])

  
  (def line-to-find (-> cljs-loc :line inc))
  (def col-to-find (-> cljs-loc :col inc))
  (def form (r/find-nested-form-closest line-to-find col-to-find top-level-form))
  (or (r/find-nested-form-closest line-to-find col-to-find top-level-form) top-level-form))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defn setup-events [c {:keys [on-break on-resume]}]
  (let [paused-chan (chrome-events/listen c :debugger :paused)
        resumed-chan (chrome-events/listen c :debugger :resumed)
        stop-chan (a/chan)
        stop-fn (fn []
                  (chrome-events/unlisten c :debugger :paused paused-chan)
                  (chrome-events/unlisten c :debugger :resumed resumed-chan)
                  (a/close! stop-chan))]
    (a/go-loop []
      (a/alt!
        paused-chan ([val]
                     (when (fn? on-break)
                       (try
                         (on-break val)
                         (catch Exception e
                           (binding [*out* *err*]
                             (println "[cljs debugger] Error in on-break handler:" e)))))
                     (recur))
        resumed-chan ([val]
                      (when (fn? on-resume)
                        (try
                          (on-resume val)
                          (catch Exception e
                            (binding [*out* *err*]
                              (println "[cljs debugger] Error in on-resume handler:" e)))))
                      (recur))
        stop-chan (println "[cljs debugger] quiting event loop")))
    {:stop stop-fn
     :resumed-chan resumed-chan
     :paused-chan paused-chan}))


(defn connect
  ([]
   (connect nil))
  ([{:keys [host port] :or {host "localhost" port 9222} :as opts}]
   (let [c (chrome/connect host port)
         chrome-events (setup-events c opts)]
     (chrome/set-current-connection! c)
     (chrome-dbg/enable c {})
     (println (str "[cljs debugger] connected to " host ":" port))
     {:chrome-connection c
      :chrome-events chrome-events})))

(defn disconnect [{:keys [chrome-connection chrome-events] :as state}]
  (when-let [stop-events (some-> chrome-events :stop)]
    (println "[cljs debugger] unsubscribing from devtools debug events")
    (stop-events))
  (when-let [websocket (some-> chrome-connection :ws-connection)]
    (println "[cljs debugger] disconnecting from devtools websocket")
    (.close websocket))
  (assoc state
         :chrome-connection nil
         :chrome-events nil))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defonce debugging-state (atom {:chrome-connection nil
                                :chrome-events nil
                                :nrepl-debug-init-msg nil}))

(defn disconnect!
  ([]
   (disconnect! @debugging-state))
  ([connection]
   (swap! debugging-state merge (disconnect connection))))


(defn connect!
  ([]
   (connect! nil))
  ([opts]
   (disconnect!)
   (swap! debugging-state merge (connect opts))))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defn enable
  [{:keys [chrome-connection] :as _debugging-state}]
  (chrome-dbg/enable chrome-connection {}))

(defn disable
  [{:keys [chrome-connection] :as _debugging-state}]
  (chrome-dbg/disable chrome-connection {}))

(defn resume
  [{:keys [chrome-connection] :as _debugging-state}]
  (chrome-dbg/resume chrome-connection {}))

(defn next
  [{:keys [chrome-connection] :as _debugging-state}]
  (chrome-dbg/step-over chrome-connection {}))

(defn quit
  [debugging-state]
  (disable debugging-state)
  (a/go (a/<! (a/timeout 500))
        (enable debugging-state)))


(comment

  (disconnect!)
  (connect!)

  (require '[clj-chrome-devtools.commands.debugger :as chrome-dbg])

  (def c (-> @debugging-state :chrome-connection))
  (chrome-dbg/resume c {})

  )
