(ns cljs-debugger.nrepl-cljs-debugger
  (:require [cljs-debugger.devtools :as devtools]
            cider.nrepl.middleware.debug
            [cider.piggieback :refer [wrap-cljs-repl]]
            [nrepl.middleware :as middleware :refer [set-descriptor!]]
            [nrepl.misc :refer [response-for]]
            [nrepl.transport :as transport]
            [cljs-debugger.reading :as r]))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; borrowed from cider.nrepl.middleware.debug
(defn debugger-send
  "Send a response through debugger-message."
  [init-debugger-message & r]
  (when (not init-debugger-message)
    (throw (Exception. "Debugger not initialized!")))
  (try
    (transport/send (:transport @init-debugger-message)
                    (apply response-for @init-debugger-message r))
    (catch java.net.SocketException _
      (reset! init-debugger-message nil))))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(def ^:private ^:dynamic *initial-debugger-message* nil)
(def ^:private ^:dynamic *devtools-connection* nil)

(defn disconnect-devtools-from-session! [session]
  (when-let [c (get @session #'*devtools-connection*)]
    (swap! session assoc #'*devtools-connection* nil)
    (devtools/disconnect! c)))

(defonce last-devtools-session (atom nil))


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
              (devtools/connect! {:on-break #(devtools/on-cljs-debugger-break % initial-debug-message)
                                  :on-resume #(devtools/on-cljs-debugger-resume % initial-debug-message)
                                  :nrepl-debug-init-msg initial-debug-message}))
       (println "[cljs debugger] devtools connection established")))))

(defn handle-cljs-debug
  [handler {:keys [op session input] :as msg}]
  ;; (println "wrap-cljs-debugger" op)
  (case op
    "init-debugger"
    (connect-devtools-with-session! session msg)

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

(defn test-debugger []

  (def init-dbg (-> @@cljs-debugger.nrepl-cljs-debugger/last-devtools-session (get #'cljs-debugger.nrepl-cljs-debugger/*initial-debugger-message*)))

  (def frame (-> @devtools/break-events first :params :call-frames first))
  (def cenv (-> init-dbg :session deref (get #'cider.piggieback/*cljs-compiler-env*)))


  ;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

  (def frame-info (devtools/form-at-frame frame cenv))

  (def  top-level-form (:top-level-form frame-info))
  (def  closure-js-file (:closure-js-file frame-info))
  (def  cljs-loc (:cljs-loc frame-info))

  (def line-to-find (-> cljs-loc :line inc))
  (def col-to-find (-> cljs-loc :col inc))

  (def form (or (r/find-nested-form-closest line-to-find col-to-find top-level-form) top-level-form))

  (def coor (-> form meta :coor))

  (def STATE__ {:code "(defn on-click [_evt]
  (let [msg \"you\"]
    (println \"on click\")
    (js-debugger)
    (js/console.log (str msg \" clicked\"))))"
                :id (-> init-dbg :id)
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

  ;; (do (reset! cider.nrepl.middleware.debug/debugger-message (-> @devtools/debugging-state :nrepl-debug-init-msg)) nil)

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


  (def f (devtools/form-at-frame frame cenv))

  (map meta forms-found)

  (clojure.walk/postwalk (juxt identity meta) f)
  cljs-loc
  f

 )
