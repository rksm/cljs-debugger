(ns cljs-debugger.cljs-debugger
  (:require [clj-chrome-devtools.commands.debugger :as chrome-dbg]
            [cljs-debugger.devtools :as devtools]
            [cljs-debugger.reading :refer [read-top-level-form-of-def-from-file]]
            [cljs.analyzer.api :as ana-api]
            [clojure.data.json :as json]))

(defonce break-events (atom []))

(defonce debugging-state (atom {:chrome-connection nil
                                :chrome-events nil
                                :nrepl-debug-init-msg nil}))



(defn disconnect! []
  (swap! debugging-state merge (devtools/disconnect @debugging-state)))


(defn connect!
  ([]
   (connect! nil))
  ([opts]
   (disconnect!)
   (swap! debugging-state merge (devtools/connect opts))
   (println "cljs debugger is connected!")))

(defn nrepl-debug-init!
  [debug-init-msg]
  (swap! debugging-state assoc :nrepl-debug-init-msg debug-init-msg))

(comment

  (disconnect!)
  (connect!)

  (require '[clj-chrome-devtools.commands.debugger :as chrome-dbg])

  (def c (-> @debugging-state :chrome-connection))
  (chrome-dbg/resume c {})

  )

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defn on-cljs-debugger-break [evt]
  (swap! break-events conj evt)
  (println "on-break" evt))

(defn on-cljs-debugger-resume [evt]
  (println "on-resume" evt))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defn form-at-frame
  "Starting from a CDP frame from a debugger break location, tries to find a cljs
  top-level form that represents the code at the break location. If found, the
  returned form will contain meta data from indexed reading as well as the
  source location in the coor vector form used by the cider debugger."
  [frame cenv]
  (let [js-file (devtools/js-file-of-frame frame cenv)
        loc (->> frame :location ((juxt :line-number :column-number)))

        sourcemap-file (devtools/source-map-of-js-file js-file)
        closure-js-file (-> @cenv :cljs.closure/compiled-cljs (get (.getCanonicalPath js-file)))
        ;; source-map-cljs-name (.getName (io/file (:source-url closure-js-file)))
        ;; script-id (-> frame :location :script-id)
        ;; call-frame-id (->> frame :call-frame-id)

        sm (json/read-json (slurp sourcemap-file) true)
        cljs-loc (apply devtools/source-map-lookup (cljs.source-map/decode sm) loc)
        ns-ast (ana-api/find-ns cenv (:ns closure-js-file))
        ;; NOTE! defs in cljs have one-indexed lines!
        defs (:defs ns-ast)
        top-level-def (let [cljs-line (inc (:line cljs-loc))]
                        (first
                         (for [[_ def] (reverse defs)
                               :when (<= (:line def) cljs-line)]
                           def)))
        top-level-form (read-top-level-form-of-def-from-file
                        top-level-def
                        (:source-url closure-js-file))]
    {:top-level-form top-level-form
     :closure-js-file closure-js-file
     :cljs-loc cljs-loc}))
