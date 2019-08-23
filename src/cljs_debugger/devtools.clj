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
            [clojure.string :as s]
            [clj-chrome-devtools.commands.runtime             :as runtime             ]))

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

(declare find-more-precise-loc-logged)

(defn source-map-lookup
  "`line-no` and `row-no` are zero-based."
  ;; [orig-cljs-file-name source-map-file line-no col-no]
  [mapping line col]
  (let [exact-row (get mapping line)
        exact-col (get exact-row col)

        exact-match (and exact-col (first exact-col))
        prev-in-row (and
                     (not exact-match)
                     (-> (for [col (range (dec col) -1 -1)
                               :let [entry (get exact-row col)]
                               :when entry]
                           entry)
                         first
                         first))
        prev-row (-> (for [row (range (dec line) -1 -1)
                           :let [entry (get mapping row)]
                           :when entry]
                       entry)
                     first vals last last)]
    (or exact-match prev-in-row prev-row)))

(defn source-map-lookup-from-file
  ""
  [js-file line col]
  (let [sourcemap-file (source-map-of-js-file js-file)
        sm (json/read-json (slurp sourcemap-file) true)
        mapping (sm/decode sm)]
    (source-map-lookup mapping line col)))

#_(defn clever-source-map-lookup
  [js-file closure-js-file line col]
  (let [sourcemap-file (source-map-of-js-file js-file)
        sm (json/read-json (slurp sourcemap-file) true)
        mapping (sm/decode sm)
        lookup (source-map-lookup mapping line col)]
    (when-let [{:keys [line col name]} lookup]
      (if-let [lookup-2 (and name
                             (find-more-precise-loc-logged
                              (:url closure-js-file) line name))]
        (do (println "--- Found better sourcemap location" lookup "vs" lookup-2 "and" (source-map-lookup mapping (:line lookup-2) (:col lookup-2)) "(" line "," col ")")
            lookup)
        lookup))))


(defn source-map-lookup-reverse
  [orig-cljs-file-name source-map-file line-no col-no]
  (let [orig-cljs-file-name-for-sm orig-cljs-file-name
        sm (sm/decode-reverse (json/read-json (slurp source-map-file) true))

        mapping (get sm orig-cljs-file-name-for-sm)]
    (source-map-lookup mapping line-no col-no)))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

#_(defn find-more-precise-loc [js-file from-line name]
  (let [token (str "var " name)
        rdr (io/reader js-file)]
    (loop [line 0]
      (let [line-string (.readLine rdr)
            found? (and line-string (s/starts-with? line-string token))]
        (cond
          (nil? line-string) nil
          found? {:line line :col 0}
          :else (recur (inc line)))))))

(defn find-more-precise-loc [js-file from-line name]
  (let [rdr (io/reader js-file)]
    (doall (repeatedly from-line #(.readLine rdr)))
    (loop [line from-line]
      (let [line-string (.readLine rdr)
            col (and line-string (s/index-of line-string name))]
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
        [line col] (->> frame :location ((juxt :line-number :column-number)))

        closure-js-file (-> @cenv :cljs.closure/compiled-cljs (get (.getCanonicalPath js-file)))
        ;; source-map-cljs-name (.getName (io/file (:source-url closure-js-file)))
        ;; script-id (-> frame :location :script-id)
        ;; call-frame-id (->> frame :call-frame-id)

        {:keys [line col] :as cljs-loc} (source-map-lookup-from-file js-file line col)
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

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(def exclude-vars-with-prefix ["G__" "c__" "temp__" "i__" "count__" "chunk__" "seq__"])

(defn- demunge [var]
  (update var :name #(s/replace % "_" "-")))

(defn locals
  [frame-info {:keys [chrome-connection] :as _debugging-state}]
  (let [object-id (-> frame-info :frame :scope-chain first :object :object-id)
        vars (runtime/get-properties chrome-connection {:object-id object-id :generate-preview true})
        vars-filtered-1 (for [var (:result vars)
                              :let [name (:name var)]
                              :when (not-any? #(.startsWith name %) exclude-vars-with-prefix)
                              :let [[_ running-var-name running-var-n] (re-find #"(.*)_([0-9]{3,})$" name)]]
                          (if running-var-name
                            {:running-var running-var-name
                             :n running-var-n
                             :var var}
                            var))
        groups-of-running-vars (group-by :running-var vars-filtered-1)
        normal-vars (get groups-of-running-vars nil)
        running-vars (for [[name vars] (dissoc groups-of-running-vars nil)
                           :let [used (filter #(not= {:type "undefined"} (-> % :var :value)) vars)
                                 {:keys [var n]} (if (= 1 (count used)) (first used) (last (sort-by :n vars)))]]
                       (assoc var
                              :name name
                              :running-var n))]
    (into {} (for [var (concat running-vars normal-vars)
                   :let [{{:keys [description type]} :value :keys [name]} (demunge var)]]
               [name (or description type)]))))


(comment

  

  (def chrome-connection (-> @@cljs-debugger.nrepl-cljs-debugger/last-devtools-session
                             (get #'cljs-debugger.nrepl-cljs-debugger/*devtools-connection*)
                             :chrome-connection))

  (def dbg-state (-> @@cljs-debugger.nrepl-cljs-debugger/last-devtools-session
                     (get #'cljs-debugger.nrepl-cljs-debugger/*devtools-connection*)))
  
  (def frame-info (-> @@cljs-debugger.nrepl-cljs-debugger/last-devtools-session (get #'cljs-debugger.nrepl-cljs-debugger/*last-break*)))

  (locals frame-info dbg-state)

  ;; (re-find #"(.*)_([0-9]{3,})$" "col_63392")
  (->> vars :result (filter (fn [var] (not-any? (.startsWith (:name %) )))))

  (disconnect!)
  (connect!)

  (require '[clj-chrome-devtools.commands.debugger :as chrome-dbg])

  (def c (-> @debugging-state :chrome-connection))
  (chrome-dbg/resume c {})

  )
