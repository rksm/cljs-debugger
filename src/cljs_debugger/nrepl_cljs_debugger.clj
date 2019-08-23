(ns cljs-debugger.nrepl-cljs-debugger
  (:require cider.nrepl.middleware.debug
            [cider.piggieback :refer [wrap-cljs-repl]]
            [cljs-debugger.devtools :as devtools]
            [cljs-debugger.reading :as r]
            [clojure.pprint :refer [cl-format pprint]]
            [nrepl.middleware :as middleware :refer [set-descriptor!]]
            [nrepl.misc :refer [response-for]]
            [nrepl.transport :as transport]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [cider.nrepl.middleware.inspect :refer [swap-inspector!]]
            [orchard.inspect :as inspect]))

(def ^:private ^:dynamic *initial-debugger-message* nil)
(def ^:private ^:dynamic *devtools-connection* nil)
(def ^:private ^:dynamic *last-break* nil)

(defonce last-devtools-session (atom nil))
(defonce break-events (atom []))

(comment
  (-> @@last-devtools-session (get #'cljs-debugger.nrepl-cljs-debugger/*initial-debugger-message*) :id)
  (-> @@last-devtools-session (get #'cljs-debugger.nrepl-cljs-debugger/*initial-debugger-message*) :id)
  (def con (-> @@last-devtools-session
               (get #'cljs-debugger.nrepl-cljs-debugger/*devtools-connection*)))
  (devtools/enable con)
  (devtools/disable con)

  (def frame-info (-> @@last-devtools-session (get #'*last-break*)))

  ;; false {:line 27, :col 4, :source core.cljs, :name seq__35004} {:line 27, :col 4, :source core.cljs, :name chunk__35005}
  ;; false {:line 27, :col 4, :source core.cljs, :name seq__35072} {:line 27, :col 4, :source core.cljs, :name chunk__35073}

  (-> frame-info :frame :location)
  (def src (slurp (-> frame-info :closure-js-file :url)))
  (let [rdr (io/reader (-> frame-info :closure-js-file :url))]
    (doall (repeatedly 27 #(.readLine rdr)))
    (loop [row 27]
      (let [line (.readLine rdr)
            col (and line (s/index-of line "chunk__35073"))]
        (cond
          (nil? line) nil
          col {:line row :col col}
          :else (recur (inc row))))))

  ;; => {:line 31, :col 4}

  (def cenv (-> @@last-devtools-session (get #'cider.piggieback/*cljs-compiler-env*)))

  (def js-file (devtools/js-file-of-frame (:frame frame-info) cenv))
  (def loc (->> (:frame frame-info) :location ((juxt :line-number :column-number))))
  (def sourcemap-file (devtools/source-map-of-js-file js-file))

  frame-info
  (source-map-of-js-file js-file)

  (devtools/source-map-lookup (cljs.source-map/decode (clojure.data.json/read-json (slurp sourcemap-file) true)) 48 15)

  (:form (devtools/analyze-frame (:frame frame-info) cenv))



  (def ws (-> @@last-devtools-session
              (get #'cljs-debugger.nrepl-cljs-debugger/*devtools-connection*)
              :chrome-connection
              :ws-connection))

  (def field (-> ws class .getDeclaredFields (nth 2)))
  (def field (-> ws class (.getDeclaredField "session")))
  (.setAccessible field true)
  (.isOpen (.get field ws))


  (doseq [[key p] @cider.nrepl.middleware.debug/promises] (deliver p ":continue"))

  (count @cider.nrepl.middleware.debug/promises)
  (reset! cider.nrepl.middleware.debug/promises {})

  (let [devtools-connection (-> @@last-devtools-session (get #'cljs-debugger.nrepl-cljs-debugger/*initial-debugger-message*) :session deref (get #'*devtools-connection*))]
    (devtools/resume devtools-connection))
  )



;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; borrowed from cider.nrepl.middleware.debug
(defn- random-uuid-str
  "Clojure(Script) UUID generator."
  []
  (letfn [(hex [] (format "%x" (rand-int 15)))
          (nhex [n] (apply str (repeatedly n hex)))]
    (let [rhex (format "%x" (bit-or 0x8 (bit-and 0x3 ^int (rand-int 14))))]
      (str (nhex 8) "-" (nhex 4) "-4" (nhex 3)
           "-" rhex (nhex 3) "-" (nhex 12)))))

(defn pr-short
  "Like `pr-str` but limited in length and depth."
  [x]
  (binding [*print-length* (:length @cider.nrepl.middleware.debug/print-options)
            *print-level*  (:level @cider.nrepl.middleware.debug/print-options)]
    ;; TODO: Make it possible to use a random print function here
    (pr-str x)))

(defn- locals-for-message
  "Prepare a map of local variables for sending through the repl."
  [locals]
  (map (partial map pr-short) locals))

(defn debugger-send
  "Send a response through debugger-message."
  [init-debugger-message & r]
  (when (not init-debugger-message)
    (throw (Exception. "[cljs debugger] in debugger-send: debugger not initialized!")))
  (try
    (transport/send (:transport init-debugger-message)
                    (apply response-for init-debugger-message r))
    (catch java.net.SocketException _
      (println "[cljs debugger] debugger-send network error"))))

(defn- read-debug-input
  "Like `read`, but reply is sent through `debugger-message`."
  [init-debugger-message dbg-state input-type prompt]
  (let [key (random-uuid-str)
        input (promise)]
    (swap! cider.nrepl.middleware.debug/promises assoc key input)
    (debugger-send init-debugger-message
                   (-> dbg-state
                       (assoc :status :need-debug-input
                              :key key
                              :prompt prompt
                              :input-type input-type)
                       (update :locals locals-for-message)))
    (binding [*ns* (find-ns (symbol (:original-ns dbg-state)))]
      (try (read-string @input)
           (finally (swap! cider.nrepl.middleware.debug/promises dissoc key))))))

(defn- debug-inspect
  "Inspect `inspect-value`."
  [page-size inspect-value initial-debugger-message-atom]
  (binding [*print-length* nil
            *print-level* nil]
    (->> #(inspect/start (assoc % :page-size page-size) inspect-value)
         ;; FIXME
         (swap-inspector! initial-debugger-message-atom)
         :rendered pr-str)))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



(defn wait-for-input
  [frame-info initial-debug-message locals]

  (pprint locals)

  (let [{:keys [top-level-form form closure-js-file]} frame-info
        coor (-> form meta :coor)
        msg-id (-> initial-debug-message :id)
        STATE__ {:code (or (some-> top-level-form meta :source)
                           (pr-str top-level-form))
                 :id msg-id
                 :file (.getCanonicalPath (:source-file closure-js-file))
                 :line (:line (meta top-level-form))
                 :column (dec (or (:column (meta top-level-form)) 0))
                 :original-ns (:ns closure-js-file)
                 :ns (:ns closure-js-file)
                 :session-id coor
                 :coor coor
                 ;; :skip false
                 :forms nil
                 :locals locals
                 :debug-value (pr-str (-> frame-info :frame :return-value))}
        inputs    {"n" :next,
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
        debug-input (read-debug-input initial-debug-message STATE__ inputs nil)
        session (-> initial-debug-message :session)
        devtools-connection (get @session #'*devtools-connection*)]
    ;; (prn "got debug input" debug-input)
    (case debug-input
      :continue (devtools/resume devtools-connection)
      :next (devtools/next devtools-connection)
      :quit (devtools/quit devtools-connection)
      :locals (->> (debug-inspect 32 locals (get @session #'*initial-debugger-message*)))
      (do
        (cl-format true "can't deal with debug-input ~s, resuming" debug-input)
        (devtools/resume devtools-connection))
      )))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(comment
  (def frame (-> evt :params :call-frames first))
  evt
  (def cenv (-> @@last-devtools-session (get #'cider.piggieback/*cljs-compiler-env*)))
  (def frame-info (devtools/analyze-frame frame cenv))
  (:top-level-form frame-info)
  (def loc (->> frame :location ((juxt :line-number :column-number))))
  (def js-file (devtools/js-file-of-frame frame cenv))
  (def sourcemap-file (devtools/source-map-of-js-file js-file))
)

(defn- same-position? [frame-info-a frame-info-b]
  (= ((juxt :line :col) (:cljs-loc frame-info-a))
     ((juxt :line :col) (:cljs-loc frame-info-b))))

(defn on-cljs-debugger-break [evt initial-debug-message]
  (println "on-break")
  (swap! break-events conj evt)
  (let [session (-> initial-debug-message :session)
        cenv (-> @session (get #'cider.piggieback/*cljs-compiler-env*))
        devtools-connection (-> @session (get #'*devtools-connection*))
        last-frame-info (-> @session (get #'*last-break*))]
    (try
      (if-not (and cenv @cenv)
        (do
          (println "[cljs debugger] break requested but no compiler env found in session")
          (devtools/resume devtools-connection))
        (let [frame (-> evt :params :call-frames first)
              frame-info (devtools/analyze-frame frame cenv)
              locals (devtools/locals frame-info devtools-connection)]
          (swap! session assoc #'*last-break* frame-info)
          ;; (println (= (:cljs-loc last-frame-info) (:cljs-loc frame-info)) (:cljs-loc last-frame-info) (:cljs-loc frame-info))
          (if (same-position? last-frame-info frame-info)
            (do
              (println "Stopped at same location before, skipping break")
              (devtools/next devtools-connection))
            (wait-for-input frame-info initial-debug-message locals))))
      (catch Exception e
        (binding [*out* *err*] (println "error when requesting break:" e))
        (devtools/resume devtools-connection)))))


(defn on-cljs-debugger-resume [evt initial-debug-message]
  (println "on-resume" evt))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(defn disconnect-devtools-from-session! [session]
  (when-let [c (get @session #'*devtools-connection*)]
    (swap! session assoc #'*devtools-connection* nil)
    (devtools/disconnect! c)))

(defn connect-devtools-with-session!
  ([session]
   (connect-devtools-with-session! session (get @session #'*initial-debugger-message*)))
  ([session initial-debug-message]
   (disconnect-devtools-from-session! session)
   (if-not initial-debug-message
     (binding [*out* *err*]
       (println "[cljs debugger] connect-devtools-with-session! does not have access to the initial debug message. Connection failed."))
     (do
       (reset! last-devtools-session session)
       (swap! session assoc
              #'*initial-debugger-message*
              initial-debug-message
              #'*devtools-connection*
              (devtools/connect! {:on-break #(on-cljs-debugger-break % initial-debug-message)
                                  :on-resume #(on-cljs-debugger-resume % initial-debug-message)
                                  :nrepl-debug-init-msg initial-debug-message}))
       (println "[cljs debugger] devtools connection established")
       (println "[cljs debugger] debug message id " (:id initial-debug-message))))))

(defn handle-cljs-debug
  [handler {:keys [op session input] :as msg}]
  ;; (println "wrap-cljs-debugger" op)
  (case op
    "init-debugger"
    (try
      (connect-devtools-with-session! session msg)
      (println "[cljs debugger] initialized")
      (catch Exception _e
        (println "[cljs debugger] Cannot connect to chrome devtools. Is a Chrome / v8 process with open debug port running?")
        (handler msg)))

    "cljs-debugger-connect"
    (connect-devtools-with-session! session)

    "cljs-debugger-disconnect"
    (disconnect-devtools-from-session! session)

    ;; "debug-input" (when-let [pro ((-> @session ::input-promises (get (:key msg))))]
    ;;                 (deliver pro input))

    (handler msg)))


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

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

;; (defn test-debugger []

;;   (def init-dbg (-> @@cljs-debugger.nrepl-cljs-debugger/last-devtools-session (get #'cljs-debugger.nrepl-cljs-debugger/*initial-debugger-message*)))

;;   (def frame (-> @devtools/break-events first :params :call-frames first))
;;   (def cenv (-> init-dbg :session deref (get #'cider.piggieback/*cljs-compiler-env*)))


;;   ;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

;;   (def frame-info (devtools/analyze-frame frame cenv))

;;   (def  top-level-form (:top-level-form frame-info))
;;   (def  closure-js-file (:closure-js-file frame-info))
;;   (def  cljs-loc (:cljs-loc frame-info))

;;   (def line-to-find (-> cljs-loc :line inc))
;;   (def col-to-find (-> cljs-loc :col inc))

;;   (def form (or (r/find-nested-form-closest line-to-find col-to-find top-level-form) top-level-form))

;;   (def coor (-> form meta :coor))

;;   (def STATE__ {:code (pr-str top-level-form)
;;                 :id (-> init-dbg :id)
;;                 :file (.getCanonicalPath (:source-file closure-js-file))
;;                 :line (:line (meta top-level-form))
;;                 :column (dec (:column (meta top-level-form)))
;;                 :original-ns (:ns closure-js-file)
;;                 :ns (:ns closure-js-file)
;;                 :session-id coor
;;                 :coor coor
;;                 ;; :skip false
;;                 :forms nil
;;                 :locals {}
;;                 :debug-value "foo"})

;;   ;; (cider.nrepl.middleware.debug/read-debug-command coor nil {} STATE__)

;;   ;; (do (reset! cider.nrepl.middleware.debug/debugger-message (-> @devtools/debugging-state :nrepl-debug-init-msg)) nil)

;;   ;; (:transport @cider.nrepl.middleware.debug/debugger-message)

;;   ;; (response-for @cider.nrepl.middleware.debug/debugger-message {} {:foo 23})

;;   ;; (pprint (read-string "(dict \"code\" \"#dbg\\n(defn test\\n  [a b]\\n  (+ a b (quot a b)))\\n\" \"column\" 0 \"coor\" (3 1) \"debug-value\" \"4\" \"file\" \"/home/robert/projects/clojure/2019-08-18_cljs-debugger/cljs-debugger/src/cljs_debugger/core.clj\" \"id\" \"7\" \"input-type\" (dict \"C\" \"Continue\" \"c\" \"continue\" \"e\" \"eval\" \"h\" \"here\" \"i\" \"in\" \"j\" \"inject\" \"l\" \"locals\" \"n\" \"next\" \"o\" \"out\" \"p\" \"inspect\" \"q\" \"quit\" \"s\" \"stacktrace\" \"t\" \"trace\") \"key\" \"eccd093a-9d54-4621-8e13-a104701169b5\" \"line\" 7 \"locals\" ((\"a\" \"4\") (\"b\" \"3\")) \"original-id\" \"96\" \"original-ns\" \"cljs-debugger.core\" \"prompt\" nil \"session\" \"a02893d8-8ee6-4a8e-8b53-ce9ff51cd5c5\" \"status\" (\"need-debug-input\"))"))

;;   (#'cider.nrepl.middleware.debug/read-debug-input
;;    STATE__
;;    {"n" :next,
;;     "s" :stacktrace,
;;     "e" :eval,
;;     "q" :quit,
;;     "p" :inspect,
;;     "j" :inject,
;;     "C" :Continue,
;;     "t" :trace,
;;     "i" :in,
;;     "l" :locals,
;;     "h" :here,
;;     "o" :out,
;;     "c" :continue}
;;    nil)

;;   )


(comment

  (test-debugger)


  (def f (devtools/analyze-frame frame cenv))

  (map meta forms-found)

  (clojure.walk/postwalk (juxt identity meta) f)
  cljs-loc
  f

 )
