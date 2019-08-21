(ns cljs-debugger.nrepl-cljs-debugger
  (:require [cider.piggieback :refer [wrap-cljs-repl]]
            [cljs-debugger.ast :as ast]
            [cljs-debugger.cljs-debugger :as cljs-debugger]
            [cljs-debugger.devtools :as devtools]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            clojure.tools.reader
            [clojure.zip :as z]
            [nrepl.middleware :as middleware :refer [set-descriptor!]]
            [nrepl.misc :refer [response-for]]
            [nrepl.transport :as transport]))

;; (cljs-utils/grab-cljs-env @last-msg)
(def last-msg (atom nil))
(def init-debugger-message (atom nil))

(comment
 (transport/send (:transport @init-debugger-message) (response-for @init-debugger-message
                                                                   :debug-value "3",
                                                                   :original-ns "cljs-debugger.browser",
                                                                   :key "106c7500-e6b6-41ce-a37b-6c7ae8cde78a",
                                                                   :locals '(("_evt" "8")),
                                                                   :file "/home/robert/projects/clojure/2019-08-18_cljs-debugger/cljs-debugger/src/cljs_debugger/browser.cljs",
                                                                   :column 0,
                                                                   :line 9,
                                                                   :input-type
                                                                   {"n" :next,
                                                                    "s" :stacktrace,
                                                                    "e" :eval,
                                                                    "q" :quit,
                                                                    "p" :inspect,
                                                                    "j" :inject,
                                                                    "C" :Continue,
                                                                    "t" :trace,
                                                                    "i" :in,
                                                                    "l" :locals,
                                                                    "h" :here,
                                                                    "o" :out,
                                                                    "c" :continue},
                                                                   :prompt nil,
                                                                   :status :need-debug-input,
                                                                   :code "(defn on-click [_evt]
  (let [msg \"you\"]
    (js-debugger)
    (js/alert (str msg \" clicked\"))))",)))






(comment

 (def cenv (-> @last-msg :session deref (get #'cider.piggieback/*cljs-compiler-env*)))
 (def repl-opts (-> @last-msg :session deref (get #'cider.piggieback/*cljs-repl-options*)))

 (def ns-ast (cljs.analyzer.api/find-ns cenv (-> @last-msg :ns symbol)))

 (def def (first
           (let [[start-line start-col _end-line _end-col] (->> @last-msg ((juxt :start :end)) (apply concat))]
             (for [[_name {:keys [line column] :as def}] (-> ns-ast :defs)
                   :when (and (= line start-line) (= column start-col))]
               def))))

 (cljs.build.api/ns->location (-> @last-msg :ns symbol) cenv)

 (def js-file (cljs.build.api/target-file-for-cljs-ns (-> @last-msg :ns symbol) (:output-dir repl-opts)))
 (def source-map-file  (cljs-debugger.devtools/source-map-of-js-file js-file))

 (lookup-reverse "browser.cljs" source-map-file 12 4)

 (apply lookup-reverse "browser.cljs" source-map-file (-> @last-msg :start))
 (apply lookup-reverse "browser.cljs" source-map-file (-> @last-msg :pos))


 ;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

 ;; example of debugger-send message

 "debugger-send" {:debug-value "3",
                  :original-ns "cljs-debugger.nrepl",
                  :key "106c7500-e6b6-41ce-a37b-6c7ae8cde78a",
                  :locals '(("a" "8") ("b" "3")),
                  :file
                  "/home/robert/projects/clojure/2019-08-18_cljs-debugger/cljs-debugger/src/cljs_debugger/nrepl.clj",
                  :column 0,
                  :input-type
                  {"n" :next,
                   "s" :stacktrace,
                   "e" :eval,
                   "q" :quit,
                   "p" :inspect,
                   "j" :inject,
                   "C" :Continue,
                   "t" :trace,
                   "i" :in,
                   "l" :locals,
                   "h" :here,
                   "o" :out,
                   "c" :continue},
                  :prompt nil,
                  :coor [3 2],
                  :line 23,
                  :status :need-debug-input,
                  :code "#dbg\n(defn test [a b]\n  (+ a b (quot a b)))\n",
                  :original-id "2221"}
 )

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; borrowed from cider.nrepl.middleware.debug
(defn debugger-send
  "Send a response through debugger-message."
  [& r]
  (when (not @init-debugger-message)
    (throw (Exception. "Debugger not initialized!")))
  (try
    (transport/send (:transport @init-debugger-message)
                    (apply response-for @init-debugger-message r))
    (catch java.net.SocketException _
      (reset! init-debugger-message nil))))

(defonce print-options (atom nil))
(defn- initialize
  "Initialize the channel used for debug-input requests."
  [{:keys [print-options] :as msg}]
  (when (map? @init-debugger-message)
    (debugger-send :status :done))
  ;; The above is just bureaucracy. The below is important.
  (reset! @#'print-options print-options)
  (reset! init-debugger-message msg))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; FIXME: stolen frome cider
(defn merge-meta
  "Non-throwing version of (vary-meta obj merge metamap-1 metamap-2 ...).
  Like `vary-meta`, this only applies to immutable objects. For
  instance, this function does nothing on atoms, because the metadata
  of an `atom` is part of the atom itself and can only be changed
  destructively."
  {:style/indent 1}
  [obj & metamaps]
  (try
    (apply vary-meta obj merge metamaps)
    (catch Exception _e obj)))

(defn- walk-indexed
  "Walk through form calling (f coor element).
  The value of coor is a vector of indices representing element's
  address in the form. Unlike `clojure.walk/walk`, all metadata of
  objects in the form is preserved."
  ([f form] (walk-indexed [] f form))
  ([coor f form]
   (let [map-inner (fn [forms]
                     (map-indexed #(walk-indexed (conj coor %1) f %2)
                                  forms))
         ;; Maps are unordered, but we can try to use the keys (and
         ;; they're important enough that we're willing to risk
         ;; getting the position wrong).
         result (cond (map? form)  (into {} (map (fn [[k v]]
                                                   [k (walk-indexed (conj coor (pr-str k)) f v)])
                                                 form))
                      ;; Order of sets is unpredictable, unfortunately.
                      (set? form)  form
                      ;; Borrowed from clojure.walk/walk
                      (list? form) (apply list (map-inner form))
                      (instance? clojure.lang.IMapEntry form) (vec (map-inner form))
                      (seq? form)  (doall (map-inner form))
                      (coll? form) (into (empty form) (map-inner form))
                      :else form)]
     (f coor (merge-meta result (meta form))))))

(comment
  (def f (walk-indexed (fn [i f] (merge-meta f {:coor i})) '(foo (bar [baz]))))
  (clojure.walk/postwalk (juxt identity meta) f)

  (clojure.walk/postwalk
   (juxt identity meta)
   (clojure.tools.reader/read
    (clojure.tools.reader.reader-types/indexing-push-back-reader "(foo bar)")))

  (meta (clojure.tools.reader.reader-types/log-source
         (clojure.tools.reader/read-string "(foo bar)"))))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defn read-indexed
  "via tools.reader"
  [file]
  (let [file-reader (clojure.tools.reader.reader-types/indexing-push-back-reader
                     (io/reader file))]
    (take-while (partial not= ::end)
                (repeatedly #(clojure.tools.reader/read file-reader nil ::end)))))

(defn with-coors [form]
  (walk-indexed (fn [i f] (merge-meta f {:coor i})) form))


(defn read-top-level-form-of-def-from-file
  "def is a def as found in an analyzed cljs ns (via :defs). file is a cljs file."
  [def file]
  (->> file
       read-indexed
       (filter #(= (:line def) (-> % meta :line)))
       first
       with-coors))

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

        sm (clojure.data.json/read-json (slurp sourcemap-file) true)
        cljs-loc (apply devtools/source-map-lookup (cljs.source-map/decode sm) loc)
        ns-ast (cljs.analyzer.api/find-ns cenv (:ns closure-js-file))
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


(defn find-nested-form-closest
  [line-to-find col-to-find form]
  ;; rank form by dist form `col-to-find`
  (let [forms (loop [node (ast/tree-zipper form) result []]
                (if (z/end? node) result
                    (let [form (z/node node)
                          {:keys [line column]} (meta form)
                          result (if (= line line-to-find)
                                   (conj result [(java.lang.Math/abs (- column col-to-find))
                                                 form])
                                   result)]
                      (recur (z/next node) result))))]
    (->> forms
         (sort-by first <)
         (map second)
         first))
  )



;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defonce break-events (atom []))


(defn test-debugger []

  (def frame (-> @break-events first :params :call-frames first))
  (def cenv (-> @cljs-debugger/debugging-state :nrepl-debug-init-msg :session deref (get #'cider.piggieback/*cljs-compiler-env*)))

  
  ;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  (def frame-info (form-at-frame frame cenv))

  (def  top-level-form (:top-level-form frame-info))
  (def  closure-js-file (:closure-js-file frame-info))
  (def  cljs-loc (:cljs-loc frame-info))

  (def line-to-find (-> cljs-loc :line inc))
  (def col-to-find (-> cljs-loc :col inc))

  (def form (or (find-nested-form-closest line-to-find col-to-find top-level-form) top-level-form))

  (def coor (-> form meta :coor))

  (def STATE__ {:code "(defn on-click [_evt]
  (let [msg \"you\"]
    (println \"on click\")
    (js-debugger)
    (js/console.log (str msg \" clicked\"))))"
                :id (-> @cljs-debugger/debugging-state :nrepl-debug-init-msg :id)
                :file (.getCanonicalPath (:source-file closure-js-file))
                :line (:line (meta top-level-form))
                :column (dec (:column (meta top-level-form)))
                :original-ns (:ns closure-js-file)
                :ns (:ns closure-js-file)
                :session-id coor
                :coor coor
                ;; :skip false
                :forms nil
                :locals {}
                :debug-value "foo"})

  ;; (cider.nrepl.middleware.debug/read-debug-command coor nil {} STATE__)

  (do (reset! cider.nrepl.middleware.debug/debugger-message (-> @cljs-debugger/debugging-state :nrepl-debug-init-msg)) nil)

  ;; (:transport @cider.nrepl.middleware.debug/debugger-message)

  ;; (response-for @cider.nrepl.middleware.debug/debugger-message {} {:foo 23})

  ;; (pprint (read-string "(dict \"code\" \"#dbg\\n(defn test\\n  [a b]\\n  (+ a b (quot a b)))\\n\" \"column\" 0 \"coor\" (3 1) \"debug-value\" \"4\" \"file\" \"/home/robert/projects/clojure/2019-08-18_cljs-debugger/cljs-debugger/src/cljs_debugger/core.clj\" \"id\" \"7\" \"input-type\" (dict \"C\" \"Continue\" \"c\" \"continue\" \"e\" \"eval\" \"h\" \"here\" \"i\" \"in\" \"j\" \"inject\" \"l\" \"locals\" \"n\" \"next\" \"o\" \"out\" \"p\" \"inspect\" \"q\" \"quit\" \"s\" \"stacktrace\" \"t\" \"trace\") \"key\" \"eccd093a-9d54-4621-8e13-a104701169b5\" \"line\" 7 \"locals\" ((\"a\" \"4\") (\"b\" \"3\")) \"original-id\" \"96\" \"original-ns\" \"cljs-debugger.core\" \"prompt\" nil \"session\" \"a02893d8-8ee6-4a8e-8b53-ce9ff51cd5c5\" \"status\" (\"need-debug-input\"))"))

  (#'cider.nrepl.middleware.debug/read-debug-input
   STATE__
   {"n" :next,
    "s" :stacktrace,
    "e" :eval,
    "q" :quit,
    "p" :inspect,
    "j" :inject,
    "C" :Continue,
    "t" :trace,
    "i" :in,
    "l" :locals,
    "h" :here,
    "o" :out,
    "c" :continue}
   nil)

  )


(comment

  (test-debugger)


  (def f (form-at-frame frame cenv))

  (map meta forms-found)

  (clojure.walk/postwalk (juxt identity meta) f)
  cljs-loc
  f

 )




(defn on-cljs-debugger-break [evt]
  (swap! break-events conj evt)
  (println "on-break" evt))

(defn on-cljs-debugger-resume [evt]
  (println "on-resume" evt))


(defn handle-cljs-debug
  [handler {:keys [op] :as msg}]
  (println "wrap-cljs-debugger" op)
  (cond
    (= op "init-debugger")
    (do
      (println "got init debugger")
      (cljs-debugger/connect! {:on-break #(on-cljs-debugger-break %)
                               :on-resume #(on-cljs-debugger-resume %)})
      (cljs-debugger/nrepl-debug-init! msg)
      (reset! init-debugger-message msg))

    (= op "cljs-set-breakpoint")
    (do
      (transport/send (:transport msg) (response-for msg :status :done))
      (reset! last-msg msg))
    :else (handler msg)))


(defn wrap-cljs-debugger [handler]
  (fn [msg] (handle-cljs-debug handler msg)))

(set-descriptor! #'wrap-cljs-debugger
                 {:requires #{#'wrap-cljs-repl}
                  :expects #{"debug-input"}
                  :handles {"cljs-set-breakpoint" {:doc "..."
                                                   :requires {"ns" "namespace"
                                                              "start" "top-level-bounds"
                                                              "end" "top-level-bounds"}
                                                   :returns  {"status" "done"}}}})
