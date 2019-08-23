(ns cljs-debugger-example.nrepl
  (:require cider.nrepl
            cider.piggieback
            [clojure.pprint :refer [cl-format]]
            figwheel.main.api
            nrepl.core
            nrepl.server
            [cljs-debugger.nrepl-cljs-debugger :refer [wrap-cljs-debugger]]))

(defonce clj-nrepl-server (atom nil))
(defonce cljs-nrepl-server (atom nil))

(defn start-clj-nrepl-server []
  (reset! clj-nrepl-server (nrepl.server/start-server
                            :handler (apply nrepl.server/default-handler
                                            (map resolve cider.nrepl/cider-middleware))
                            :port 7888))
  (cl-format true "clj nrepl server started~%"))

(defn start-cljs-nrepl-server []
  (reset! cljs-nrepl-server (nrepl.server/start-server
                             :handler (apply nrepl.server/default-handler
                                             (conj
                                              (map resolve cider.nrepl/cider-middleware)
                                              #'cider.piggieback/wrap-cljs-repl
                                              #'wrap-cljs-debugger))
                             :port 7889))
  (cl-format true "cljs nrepl server started~%"))

(defn restart-cljs-server []
  (when @cljs-nrepl-server
    (nrepl.server/stop-server @cljs-nrepl-server))
  (require 'figwheel.main.api)
  (try (figwheel.main.api/stop-all) (catch Exception e (prn e)))
  (start-cljs-nrepl-server))

(defn -main []
  (start-clj-nrepl-server)
  (start-cljs-nrepl-server))

(comment
  (restart-cljs-server)
  )
